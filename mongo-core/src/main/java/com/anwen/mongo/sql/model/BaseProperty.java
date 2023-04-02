package com.anwen.mongo.sql.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 10:42
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class BaseProperty {
    /**
     * mongodb地址
     * @author JiaChaoYang
     * @since 2023/2/10 13:45
     */
    private String host;

    /**
     * mongodb端口
     * @since 2023/2/10 13:45
     */
    private String port;

    /**
     * TODO mongodb数据库名称，第二版本需要多数据源，可随时切换
     * @since 2023/2/10 13:45
     */
    private String database;

    /**
     * 用户名
     * @author JiaChaoYang
     * @date 2023/2/23 10:43
    */
    private String username;

    /**
     * 密码
     * @author JiaChaoYang
     * @date 2023/2/23 10:43
    */
    private String password;

    /**
     * 如果开启了认证，清在这里配置认证数据库
     * @author: JiaChaoYang
     * @date: 2023/3/1 22:07
     **/
    private String authenticationDatabase;

    /**
     * 指定单个连接池中随时必须存在的最小连接数 默认0
     * @author: JiaChaoYang
     * @date: 2023/2/26 20:52
     **/
    private Integer minPoolSize;

    /**
     * 指定连接池在给定时间可以拥有的最大连接数 默认100
     * @author: JiaChaoYang
     * @date: 2023/2/26 20:53
     **/
    private Integer maxPoolSize;

    /**
     * 指定线程等待连接可用的最长时间（以毫秒为单位），默认120秒
     * @author: JiaChaoYang
     * @date: 2023/2/26 20:54
     **/
    private Integer waitQueueTimeoutMS;

    /**
     * 指定驱动程序在引发异常之前等待服务器选择成功的最长时间（毫秒） 默认30秒
     * @author: JiaChaoYang
     * @date: 2023/2/26 20:57
     **/
    private Integer serverSelectionTimeoutMS;

    /**
     * 当与副本集中的多个MongoDB实例通信时，驱动程序只会向响应时间小于或等于响应时间最快的服务器加上本地阈值（以毫秒为单位）的服务器发送请求，默认15
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:06
     **/
    private Integer localThresholdMS;

    /**
     * 指定驱动程序在尝试确定群集中每个服务器的当前状态之间等待的频率（以毫秒为单位） 默认10毫秒
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer heartbeatFrequencyMS;

    /**
     * 指定提供的连接字符串包括多个主机。如果指定，驱动程序将尝试查找该集合的所有成员 默认null
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String replicaSet;

    /**
     * 指定与MongoDB实例的所有通信都应使用TLS/SSL。被tls选项取代 默认false
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean ssl;

    /**
     * 指定与MongoDB实例的所有通信都应使用TLS。取代ssl选项 默认false
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean tls;

    /**
     * 指定驱动程序应允许TLS连接的无效主机名。与将tlsAllowInvalidHostnames设置为true具有相同的效果。要以其他方式配置TLS安全约束，请使用自定义SSLContext 默认false
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean tlsInsecure;

    /**
     * 指定驱动程序应允许TLS连接的证书中包含无效的主机名。取代允许的sslInvalidHostName 默认false
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean tlsAllowInvalidHostnames;

    /**
     * 指定Java驱动程序在超时之前等待连接打开的最长时间（以毫秒为单位）。值0指示驱动程序在等待连接打开时永不超时 默认10毫秒
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer connectTimeoutMS;

    /**
     * 指定Java驱动程序在超时之前等待发送或接收请求的最长时间（以毫秒为单位）。值0指示驱动程序在等待发送或接收请求时永不超时 默认0
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer socketTimeoutMS;

    /**
     * 指定Java驱动程序在关闭连接之前允许池连接空闲的最长时间（以毫秒为单位）。值0表示驱动程序允许池集合空闲的时间没有上限 默认0
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer maxIdleTimeMS;

    /**
     * 指定关闭连接之前，Java驱动程序将继续使用池连接的最长时间（以毫秒为单位）。值为0表示驱动程序可以保持池连接打开的时间没有上限 默认0
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer maxLifeTimeMS;

    /**
     * 指定驱动程序必须等待连接的MongoDB实例对磁盘上的日志文件进行组提交，以执行所有写入操作 默认false
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean journal;

    /**
     * 指定写入问题。有关值的更多信息，请参阅官方文档 默认1
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String w;

    /**
     * 指定写入问题的时间限制（以毫秒为单位）。有关详细信息，请参阅wtimeoutMS选项。值0指示驱动程序从不超时写入操作 默认0
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer wtimeoutMS;

    /**
     * 指定读取首选项。有关值的更多信息，请参阅readPreference选项 默认primary
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String readPreference;

    /**
     * 指定读取首选项标记。有关值的更多信息，请参阅readPreferenceTags选项 默认null
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String readPreferenceTags;

    /**
     * 指定在驱动程序停止与辅助设备通信之前，辅助设备的过时程度（以秒为单位）。最小值为90秒或心跳频率加10秒，以较大者为准。
     * 有关详细信息，请参阅maxStalenessSeconds选项。不提供参数或显式指定-1表示不应对辅助设备进行过时检查 默认-1
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer maxStalenessSeconds;

    /**
     * 指定驱动程序在提供凭据时应使用的身份验证机制 默认情况下，客户端根据服务器版本选择最安全的机制。有关可能的值，请参阅authMechanism选项
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String authMechanism;

    /**
     * 指定应验证提供的凭据的数据库 默认admin
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:29
     **/
    private String authSource;

    /**
     * 将指定身份验证机制的身份验证财产指定为以冒号分隔的财产和值的列表。有关详细信息，请参阅authMechanismProperties选项 默认null
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String authMechanismProperties;

    /**
     * 指定在连接握手期间提供给MongoDB实例的应用程序的名称。可用于服务器日志和分析 默认null
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String appName;

    /**
     * 指定驱动程序将尝试使用的一个或多个压缩算法来压缩发送到连接的MongoDB实例的请求。可能的值包括：zlib、snappy和zstd 默认null
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String compressors;

    /**
     * 指定Zlib的压缩程度 应该使用来减少对连接的MongoDB实例的请求大小。级别可以从-1到9，较低的值压缩得更快（但会导致较大的请求），较大的值压缩速度较慢（但会造成较小的请求） 默认null
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer zlibCompressionLevel;

    /**
     * 指定如果由于网络错误而失败，驱动程序必须重试支持的写入操作默认true
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean retryWrites;

    /**
     * 指定如果由于网络错误导致读取操作失败，驱动程序必须重试支持的读取操作 默认true
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean retryReads;

    /**
     * 指定用于读取和写入操作的UUID表示形式。有关详细信息，请参阅MongoClientSettings.getUuidRepresentation（）方法 默认unspecified
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String uuidRepresentation;

    /**
     * 指定驱动程序必须直接连接到主机 默认false
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Boolean directConnection;

    /**
     * 指定池可以同时建立的最大连接数 默认2
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private Integer maxConnecting;

    /**
     * 指定驱动程序检索以构建种子列表的SRV资源记录的服务名称。要使用此选项，必须在连接URI中使用DNS种子列表连接格式 默认mongodb
     * @author: JiaChaoYang
     * @date: 2023/2/26 21:11
     **/
    private String srvServiceName;
}
