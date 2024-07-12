package com.anwen.mongo.config;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.cache.global.MongoPlusClientCache;
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
import com.anwen.mongo.property.MongoDBCollectionProperty;
import com.anwen.mongo.property.MongoDBConfigurationProperty;
import com.anwen.mongo.property.MongoDBConnectProperty;
import com.anwen.mongo.tenant.TenantAspect;
import com.anwen.mongo.toolkit.CollUtil;
import com.anwen.mongo.transactional.MongoTransactionalAspect;
import com.mongodb.client.MongoClient;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.stream.Collectors;

import static com.anwen.mongo.toolkit.MongoUtil.getMongo;

/**
 * @author JiaChaoYang
 * 连接配置
 * @since 2023-02-09 14:27
 **/
@EnableConfigurationProperties(value = {MongoDBConnectProperty.class, MongoDBCollectionProperty.class, MongoDBConfigurationProperty.class})
public class MongoPlusConfiguration {

    private final MongoDBConnectProperty mongoDBConnectProperty;

    private final MongoDBConfigurationProperty mongoDBConfigurationProperty;

    public MongoPlusConfiguration(MongoDBConnectProperty mongodbConnectProperty, MongoDBConfigurationProperty mongodbConfigurationProperty) {
        this.mongoDBConnectProperty = mongodbConnectProperty;
        this.mongoDBConfigurationProperty = mongodbConfigurationProperty;
    }

    /**
     * 注册MongoClient工厂
     * @return {@link MongoClientFactory}
     * @author anwen
     * @date 2024/5/27 下午11:21
     */
    @Bean
    @ConditionalOnMissingBean
    public MongoClientFactory mongoClientFactory(){
        MongoClientFactory mongoClientFactory = MongoClientFactory.getInstance(getMongo(DataSourceConstant.DEFAULT_DATASOURCE,mongoDBConnectProperty));
        if (CollUtil.isNotEmpty(mongoDBConnectProperty.getSlaveDataSource())){
            mongoDBConnectProperty.getSlaveDataSource().forEach(slaveDataSource -> mongoClientFactory.addMongoClient(slaveDataSource.getSlaveName(),getMongo(slaveDataSource.getSlaveName(),slaveDataSource)));
        }
        return mongoClientFactory;
    }

    /**
     * 这里将MongoClient注册为Bean，但是只是给MongoTemplate使用，master的client
     * @author JiaChaoYang
     * @date 2024/1/4 23:49
     */
    @Bean
    @ConditionalOnMissingBean
    public MongoClient mongo(MongoClientFactory mongoClientFactory){
        return mongoClientFactory.getMongoClient();
    }

    /**
     * MongoPlusClient注册
     * @param mongo MongoClient
     * @param mongoClientFactory MongoClient工厂
     * @return {@link com.anwen.mongo.manager.MongoPlusClient}
     * @author anwen
     * @date 2024/5/27 下午11:20
     */
    @Bean
    @ConditionalOnMissingBean(MongoPlusClient.class)
    public MongoPlusClient mongoPlusClient(MongoClient mongo,MongoClientFactory mongoClientFactory){
        MongoPlusClient mongoPlusClient = Configuration.builder().initMongoPlusClient(mongo,mongoDBConnectProperty);
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
            System.out.println(":: MongoPlus ::                        (v2.1.1)");
        }
        return mongoPlusClient;
    }

    /**
     * 简单类型注册
     * @return {@link SimpleTypeHolder}
     * @author anwen
     * @date 2024/5/27 下午11:17
     */
    @Bean
    @ConditionalOnMissingBean(SimpleTypeHolder.class)
    public SimpleTypeHolder simpleTypeHolder(){
        return new SimpleTypeHolder();
    }

    /**
     * 注册mongo转换器
     * @param mongoPlusClient MongoPlusClient
     * @param simpleTypeHolder 简单类型
     * @return {@link com.anwen.mongo.mapping.MongoConverter}
     * @author anwen
     * @date 2024/5/27 下午11:17
     */
    @Bean
    @ConditionalOnMissingBean(MongoConverter.class)
    public MongoConverter mongoConverter(MongoPlusClient mongoPlusClient,SimpleTypeHolder simpleTypeHolder){
        return new MappingMongoConverter(mongoPlusClient,simpleTypeHolder);
    }

    /**
     * baseMapper注册
     * @param mongoPlusClient mongoPlusClient
     * @param mongoConverter 转换器
     * @return {@link com.anwen.mongo.mapper.BaseMapper}
     * @author anwen
     * @date 2024/5/27 下午11:18
     */
    @Bean
    @ConditionalOnMissingBean(BaseMapper.class)
    public BaseMapper mongoBaseMapper(MongoPlusClient mongoPlusClient, MongoConverter mongoConverter){
        return new DefaultBaseMapperImpl(mongoPlusClient,mongoConverter);
    }

    /**
     * 注册MongoPlusMapMapper
     * @param mongoPlusClient mongoPlusClient
     * @param mongoConverter 转换器
     * @return {@link com.anwen.mongo.mapper.MongoPlusMapMapper}
     * @author anwen
     * @date 2024/5/27 下午11:19
     */
    @Bean("mongoPlusMapMapper")
    @ConditionalOnMissingBean
    public MongoPlusMapMapper mongoPlusMapMapper(MongoPlusClient mongoPlusClient,MongoConverter mongoConverter) {
        return new MongoPlusMapMapper(mongoPlusClient,mongoConverter);
    }

    /**
     * 注册MongoPlus事务切面
     * @return {@link MongoTransactionalAspect}
     * @author anwen
     * @date 2024/5/27 下午11:19
     */
    @Bean("mongoTransactionalAspect")
    @ConditionalOnMissingBean
    public MongoTransactionalAspect mongoTransactionalAspect() {
        return new MongoTransactionalAspect();
    }

    /**
     * 注册mongoPlus多数据源切面
     * @param
     * @return {@link MongoDataSourceAspect}
     * @author anwen
     * @date 2024/5/27 下午11:19
     */
    @Bean("mongoDataSourceAspect")
    @ConditionalOnMissingBean
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
    @ConditionalOnMissingBean
    public MongoLogicIgnoreAspect mongoLogicIgnoreAspect() {
        return new MongoLogicIgnoreAspect();
    }

    /**
     * 忽略租户
     * @author anwen
     * @date 2024/6/27 下午1:30
     */
    @Bean("tenantAspect")
    @ConditionalOnMissingBean
    public TenantAspect tenantAspect(){
        return new TenantAspect();
    }

    /**
     * 数据源管理器
     * @param mongoPlusClient mongoPlus客户端
     * @param mongoClientFactory mongoClient工厂
     * @return {@link DataSourceManager}
     * @author anwen
     * @date 2024/7/9 下午4:41
     */
    @Bean("dataSourceManager")
    @ConditionalOnMissingBean
    public DataSourceManager dataSourceManager(MongoPlusClient mongoPlusClient,
                                               MongoClientFactory mongoClientFactory){
        return new DataSourceManager(mongoPlusClient,mongoClientFactory);
    }

}
