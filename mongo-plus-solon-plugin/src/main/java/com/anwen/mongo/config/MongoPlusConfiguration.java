package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.cache.global.MongoPlusClientCache;
import com.anwen.mongo.cache.global.SimpleCache;
import com.anwen.mongo.conn.CollectionManager;
import com.anwen.mongo.constant.DataSourceConstant;
import com.anwen.mongo.datasource.MongoDataSourceAspect;
import com.anwen.mongo.factory.MongoClientFactory;
import com.anwen.mongo.logic.MongoLogicIgnoreAspect;
import com.anwen.mongo.manager.DataSourceManager;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.mapper.DefaultBaseMapperImpl;
import com.anwen.mongo.mapper.MongoPlusMapMapper;
import com.anwen.mongo.mapping.MappingMongoConverter;
import com.anwen.mongo.mapping.MongoConverter;
import com.anwen.mongo.mapping.SimpleTypeHolder;
import com.anwen.mongo.property.*;
import com.anwen.mongo.tenant.TenantAspect;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.client.MongoClient;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Condition;
import org.noear.solon.annotation.Configuration;
import org.noear.solon.annotation.Inject;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.anwen.mongo.toolkit.MongoUtil.getMongo;

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

    @Inject("${mongo-plus}")
    private MongoDBLogProperty mongoDBLogProperty;

    /**
     * 将MongoClient注册为Bean
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
     */
    @Bean
    @Condition(onMissingBean = MongoClient.class)
    public MongoClient mongo(MongoClientFactory mongoClientFactory){
        return mongoClientFactory.getMongoClient();
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


    @Bean
    @Condition(onMissingBean = MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongo,MongoClientFactory mongoClientFactory){
        mongoDBConfigurationProperty = Optional.ofNullable(mongoDBConfigurationProperty).orElseGet(MongoDBConfigurationProperty::new);
        MongoPlusClient mongoPlusClient = com.anwen.mongo.config.Configuration.builder().initMongoPlusClient(mongo,mongoDBConnectProperty);
        mongoClientFactory.getMongoClientMap().forEach((ds,mongoClient) -> mongoPlusClient.getCollectionManagerMap().put(ds,new LinkedHashMap<String, CollectionManager>(){{
            String database = DataSourceNameCache.getBaseProperty(ds).getDatabase();
            Arrays.stream(database.split(",")).collect(Collectors.toList()).forEach(db -> put(db,new CollectionManager(db)));
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
            System.out.println(":: MongoPlus ::                        (v2.1.2)");
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
        SimpleTypeHolder simpleTypeHolder = new SimpleTypeHolder();
        SimpleCache.setSimpleTypeHolder(simpleTypeHolder);
        return simpleTypeHolder;
    }

    @Bean
    @Condition(onMissingBean = MongoConverter.class)
    public MongoConverter mongoConverter(MongoPlusClient mongoPlusClient) {
        return new MappingMongoConverter(mongoPlusClient);
    }

    @Bean("mongoTransactionalAspect")
    @Deprecated
    @Condition(onMissingBean = MongoTransactionalAspect.class)
    public MongoTransactionalAspect mongoTransactionalAspect(MongoClient mongoClient) {
        return new MongoTransactionalAspect(mongoClient);
    }

    @Bean
    public MongoPlusAutoConfiguration mongoPlusAutoConfiguration(@Inject BaseMapper baseMapper,
                                                                 @Inject(value = "${mongo-plus.configuration.logic}",required = false) MongoLogicDelProperty mongoLogicDelProperty){
        return new MongoPlusAutoConfiguration(baseMapper,mongoDBLogProperty,mongoDBCollectionProperty,mongoLogicDelProperty);
    }

    @Bean
    @Condition(onMissingBean = BaseMapper.class)
    public BaseMapper baseMapper(MongoPlusClient mongoPlusClient,MongoConverter mongoConverter){
        return new DefaultBaseMapperImpl(mongoPlusClient,mongoConverter);
    }

    /**
     * 数据源管理器
     * @param mongoPlusClient mongoPlus客户端
     * @param mongoClientFactory mongoClient工厂
     * @return {@link DataSourceManager}
     * @author anwen
     * @date 2024/7/9 下午4:41
     */
    @Bean
    @Condition(onMissingBean = DataSourceManager.class)
    public DataSourceManager dataSourceManager(MongoPlusClient mongoPlusClient,
                                               MongoClientFactory mongoClientFactory){
        return new DataSourceManager(mongoPlusClient,mongoClientFactory);
    }

    /**
     * 注册mongoPlus多数据源切面
     * @param
     * @return {@link MongoDataSourceAspect}
     * @author anwen
     * @date 2024/5/27 下午11:19
     */
    @Bean("mongoDataSourceAspect")
    @Condition(onMissingBean = MongoDataSourceAspect.class)
    public MongoDataSourceAspect mongoDataSourceAspect() {
        return new MongoDataSourceAspect();
    }

    /**
     * 忽略逻辑删除
     *
     * @return {@link MongoLogicIgnoreAspect}
     * @author loser
     */
    @Bean("mongoLogicIgnoreAspect")
    @Condition(onMissingBean = MongoLogicIgnoreAspect.class)
    public MongoLogicIgnoreAspect mongoLogicIgnoreAspect() {
        return new MongoLogicIgnoreAspect();
    }

    /**
     * 忽略租户
     * @author anwen
     * @date 2024/6/27 下午1:30
     */
    @Bean("tenantAspect")
    @Condition(onMissingBean = TenantAspect.class)
    public TenantAspect tenantAspect(){
        return new TenantAspect();
    }

}
