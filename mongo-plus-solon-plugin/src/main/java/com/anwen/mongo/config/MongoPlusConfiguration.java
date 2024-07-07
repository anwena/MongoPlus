package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.convert.CollectionNameConvert;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.listener.BaseListener;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.DefaultBaseMapperImpl;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.mapping.MappingMongoConverter;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.SimpleTypeHolder;
import com.anwen.mongo.model.BaseProperty;
import com.anwen.mongo.property.*;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.toolkit.MongoCollectionUtils;
import com.anwen.mongo.toolkit.UrlJoint;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.connection.SslSettings;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Condition;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.*;
import java.security.cert.CertificateException;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@Configuration
public class MongoPlusConfiguration {

    @Inject("${mongo-plus.data.mongodb}")
    private MongoDBConnectProperty mongoDBConnectProperty;

    @Inject(value = "${mongo-plus.configuration.collection}",required = false)
    private MongoDBCollectionProperty mongoDBCollectionProperty;

    @Inject(value = "${mongo-plus.configuration}",required = false)
    private MongoDBConfigurationProperty mongoDBConfigurationProperty;

    /**
     * 将MongoClient注册为Bean
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
     */
    @Bean
    @Condition(onMissingBean = MongoClient.class)
    public MongoClient mongo(){
        return MongoClients.create(MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(new UrlJoint(mongoDBConnectProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseListener())).build());
    }

    @Bean
    @Condition(onMissingBean = MongoClientFactory.class)
    public MongoClientFactory mongoClientFactory(){
        MongoClientFactory mongoClientFactory = MongoClientFactory.getInstance(getMongo(DataSourceConstant.DEFAULT_DATASOURCE,mongoDBConnectProperty));
        if (CollUtil.isNotEmpty(mongoDBConnectProperty.getSlaveDataSource())){
            mongoDBConnectProperty.getSlaveDataSource().forEach(slaveDataSource -> mongoClientFactory.addMongoClient(slaveDataSource.getSlaveName(),getMongo(slaveDataSource.getSlaveName(),slaveDataSource)));
        }
        return mongoClientFactory;
    }

    /**
     * 建立连接
     * @param dsName 数据源名称
     * @param baseProperty 配置
     * @return {@link MongoClient}
     * @author anwen
     * @date 2024/5/27 下午11:20
     */
    public MongoClient getMongo(String dsName,BaseProperty baseProperty){
        DataSourceNameCache.setBaseProperty(dsName,baseProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder();
        if (baseProperty.getSsl()){
            try {
                // 加载客户端密钥库
                KeyStore clientKeyStore = KeyStore.getInstance("JKS");
                clientKeyStore.load(Files.newInputStream(Paths.get(baseProperty.getClientKeyStore())), baseProperty.getKeyPassword().toCharArray());

                // 初始化KeyManager
                KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
                keyManagerFactory.init(clientKeyStore, baseProperty.getKeyPassword().toCharArray());

                // 加载信任库
                KeyStore trustStore = KeyStore.getInstance("JKS");
                trustStore.load(Files.newInputStream(Paths.get(baseProperty.getJks())), baseProperty.getKeyPassword().toCharArray());

                // 初始化TrustManager
                TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance("SunX509");
                trustManagerFactory.init(trustStore);

                // 初始化SSL上下文
                SSLContext sslContext = SSLContext.getInstance("TLS");
                sslContext.init(keyManagerFactory.getKeyManagers(), trustManagerFactory.getTrustManagers(), null);
                // 配置MongoClientOptions以使用SSL
                SslSettings sslSettings = SslSettings.builder()
                        .invalidHostNameAllowed(baseProperty.isInvalidHostNameAllowed())
                        .context(sslContext)
                        .build();
                builder.applyToSslSettings(ssl -> ssl.applySettings(sslSettings));
            } catch (NoSuchAlgorithmException | KeyManagementException | CertificateException | KeyStoreException |
                     IOException | UnrecoverableKeyException e) {
                throw new RuntimeException(e);
            }
        }
        builder.applyConnectionString(new ConnectionString(new UrlJoint(baseProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseListener()));
        return MongoClients.create(builder.build());
    }

    @Bean
    @Condition(onMissingBean = CollectionNameConvert.class)
    public CollectionNameConvert collectionNameConvert(){
        mongoDBCollectionProperty = Optional.ofNullable(mongoDBCollectionProperty).orElseGet(MongoDBCollectionProperty::new);
        return MongoCollectionUtils.build(mongoDBCollectionProperty.getMappingStrategy());
    }


    @Bean
    @Condition(onMissingBean = MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongo,CollectionNameConvert collectionNameConvert,MongoClientFactory mongoClientFactory){
        mongoDBConfigurationProperty = Optional.ofNullable(mongoDBConfigurationProperty).orElseGet(MongoDBConfigurationProperty::new);
        MongoPlusClient mongoPlusClient = com.anwen.mongo.config.Configuration.builder().initMongoPlusClient(mongo, collectionNameConvert, mongoDBConnectProperty);
        mongoClientFactory.getMongoClientMap().forEach((ds,mongoClient) -> mongoPlusClient.getCollectionManagerMap().put(ds,new LinkedHashMap<String, CollectionManager>(){{
            String database = DataSourceNameCache.getBaseProperty(ds).getDatabase();
            Arrays.stream(database.split(",")).collect(Collectors.toList()).forEach(db -> put(db,new CollectionManager(mongoClient,collectionNameConvert,db)));
        }}));
        MongoPlusClientCache.mongoPlusClient = mongoPlusClient;
        if (mongoDBConfigurationProperty.getBanner()){
            // 参考 Easy-ES
            if (mongoDBConfigurationProperty.getIkun()){
                System.out.println("                 鸡你太美\n" +
                        "               鸡你实在太美\n" +
                        "                鸡你是太美\n" +
                        "                 鸡你太美\n" +
                        "              实在是太美鸡你\n" +
                        "         鸡你 实在是太美鸡你 美\n" +
                        "       鸡你  实在是太美鸡美   太美\n" +
                        "      鸡你  实在是太美鸡美      太美\n" +
                        "    鸡你    实在是太美鸡美       太美\n" +
                        "   鸡你    鸡你实在是美太美    美蓝球球球\n" +
                        "鸡 鸡     鸡你实在是太美     篮球篮球球球球\n" +
                        " 鸡      鸡你太美裆鸡太啊     球球蓝篮球球\n" +
                        "         鸡你太美裆裆鸡美       球球球\n" +
                        "          鸡你裆小 j 鸡太美\n" +
                        "           鸡太美    鸡太美\n" +
                        "            鸡美      鸡美\n" +
                        "            鸡美       鸡美\n" +
                        "             鸡美       鸡美\n" +
                        "             鸡太       鸡太\n" +
                        "           鸡 脚       鸡 脚\n" +
                        "           皮 鞋       皮 鞋\n" +
                        "       金光 大道         金光 大道\n" +
                        "      鸡神保佑       永不宕机     永无BUG");
            }else {
                System.out.println("___  ___                       ______ _           \n" +
                        "|  \\/  |                       | ___ \\ |          \n" +
                        "| .  . | ___  _ __   __ _  ___ | |_/ / |_   _ ___ \n" +
                        "| |\\/| |/ _ \\| '_ \\ / _` |/ _ \\|  __/| | | | / __|\n" +
                        "| |  | | (_) | | | | (_| | (_) | |   | | |_| \\__ \\\n" +
                        "\\_|  |_/\\___/|_| |_|\\__, |\\___/\\_|   |_|\\__,_|___/\n" +
                        "                     __/ |                        \n" +
                        "                    |___/                         ");
            }
            System.out.println(":: MongoPlus ::                        (v2.1.0)");
        }
        return mongoPlusClient;
    }

    @Bean
    @Condition(onMissingBean = MongoPlusMapMapper.class)
    public MongoPlusMapMapper mongoPlusMapMapper(MongoPlusClient mongoPlusClient,MongoConverter mongoConverter) {
        return new MongoPlusMapMapper(mongoPlusClient,mongoConverter);
    }

    @Bean
    @Condition(onMissingBean = SimpleTypeHolder.class)
    public SimpleTypeHolder simpleTypeHolder() {
        return new SimpleTypeHolder();
    }

    @Bean
    @Condition(onMissingBean = MongoConverter.class)
    public MongoConverter mongoConverter(MongoPlusClient mongoPlusClient,SimpleTypeHolder simpleTypeHolder) {
        return new MappingMongoConverter(mongoPlusClient,simpleTypeHolder);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    @Condition(onMissingBean = MongoTransactionalAspect.class)
    public MongoTransactionalAspect mongoTransactionalAspect(MongoClient mongoClient) {
        return new MongoTransactionalAspect(mongoClient);
    }

    @Bean
    public MongoPlusAutoConfiguration mongoPlusAutoConfiguration(@Inject BaseMapper baseMapper,
                                                                 @Inject("${mongo-plus}") MongoDBLogProperty mongoDBLogProperty,
                                                                 @Inject(value = "${mongo-plus.configuration.logic}",required = false) MongoLogicDelProperty mongoLogicDelProperty){
        return new MongoPlusAutoConfiguration(baseMapper,mongoDBLogProperty,mongoDBCollectionProperty,mongoLogicDelProperty);
    }

    @Bean
    @Condition(onMissingBean = BaseMapper.class)
    public BaseMapper baseMapper(MongoPlusClient mongoPlusClient,MongoConverter mongoConverter){
        return new DefaultBaseMapperImpl(mongoPlusClient,mongoConverter);
    }

}
