package com.anwen.mongo.model;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 10:42
 **/
public class BaseProperty {

    private String url;

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
     * mongodb数据库名称
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
     * 如果开启了认证，请在这里配置认证数据库
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

    public String getHost() {
        return this.host;
    }

    public String getPort() {
        return this.port;
    }

    public String getDatabase() {
        return this.database;
    }

    public String getUsername() {
        return this.username;
    }

    public String getPassword() {
        return this.password;
    }

    public String getAuthenticationDatabase() {
        return this.authenticationDatabase;
    }

    public Integer getMinPoolSize() {
        return this.minPoolSize;
    }

    public Integer getMaxPoolSize() {
        return this.maxPoolSize;
    }

    public Integer getWaitQueueTimeoutMS() {
        return this.waitQueueTimeoutMS;
    }

    public Integer getServerSelectionTimeoutMS() {
        return this.serverSelectionTimeoutMS;
    }

    public Integer getLocalThresholdMS() {
        return this.localThresholdMS;
    }

    public Integer getHeartbeatFrequencyMS() {
        return this.heartbeatFrequencyMS;
    }

    public String getReplicaSet() {
        return this.replicaSet;
    }

    public Boolean getSsl() {
        return this.ssl;
    }

    public Boolean getTls() {
        return this.tls;
    }

    public Boolean getTlsInsecure() {
        return this.tlsInsecure;
    }

    public Boolean getTlsAllowInvalidHostnames() {
        return this.tlsAllowInvalidHostnames;
    }

    public Integer getConnectTimeoutMS() {
        return this.connectTimeoutMS;
    }

    public Integer getSocketTimeoutMS() {
        return this.socketTimeoutMS;
    }

    public Integer getMaxIdleTimeMS() {
        return this.maxIdleTimeMS;
    }

    public Integer getMaxLifeTimeMS() {
        return this.maxLifeTimeMS;
    }

    public Boolean getJournal() {
        return this.journal;
    }

    public String getW() {
        return this.w;
    }

    public Integer getWtimeoutMS() {
        return this.wtimeoutMS;
    }

    public String getReadPreference() {
        return this.readPreference;
    }

    public String getReadPreferenceTags() {
        return this.readPreferenceTags;
    }

    public Integer getMaxStalenessSeconds() {
        return this.maxStalenessSeconds;
    }

    public String getAuthMechanism() {
        return this.authMechanism;
    }

    public String getAuthSource() {
        return this.authSource;
    }

    public String getAuthMechanismProperties() {
        return this.authMechanismProperties;
    }

    public String getAppName() {
        return this.appName;
    }

    public String getCompressors() {
        return this.compressors;
    }

    public Integer getZlibCompressionLevel() {
        return this.zlibCompressionLevel;
    }

    public Boolean getRetryWrites() {
        return this.retryWrites;
    }

    public Boolean getRetryReads() {
        return this.retryReads;
    }

    public String getUuidRepresentation() {
        return this.uuidRepresentation;
    }

    public Boolean getDirectConnection() {
        return this.directConnection;
    }

    public Integer getMaxConnecting() {
        return this.maxConnecting;
    }

    public String getSrvServiceName() {
        return this.srvServiceName;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public void setPort(String port) {
        this.port = port;
    }

    public void setDatabase(String database) {
        this.database = database;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setAuthenticationDatabase(String authenticationDatabase) {
        this.authenticationDatabase = authenticationDatabase;
    }

    public void setMinPoolSize(Integer minPoolSize) {
        this.minPoolSize = minPoolSize;
    }

    public void setMaxPoolSize(Integer maxPoolSize) {
        this.maxPoolSize = maxPoolSize;
    }

    public void setWaitQueueTimeoutMS(Integer waitQueueTimeoutMS) {
        this.waitQueueTimeoutMS = waitQueueTimeoutMS;
    }

    public void setServerSelectionTimeoutMS(Integer serverSelectionTimeoutMS) {
        this.serverSelectionTimeoutMS = serverSelectionTimeoutMS;
    }

    public void setLocalThresholdMS(Integer localThresholdMS) {
        this.localThresholdMS = localThresholdMS;
    }

    public void setHeartbeatFrequencyMS(Integer heartbeatFrequencyMS) {
        this.heartbeatFrequencyMS = heartbeatFrequencyMS;
    }

    public void setReplicaSet(String replicaSet) {
        this.replicaSet = replicaSet;
    }

    public void setSsl(Boolean ssl) {
        this.ssl = ssl;
    }

    public void setTls(Boolean tls) {
        this.tls = tls;
    }

    public void setTlsInsecure(Boolean tlsInsecure) {
        this.tlsInsecure = tlsInsecure;
    }

    public void setTlsAllowInvalidHostnames(Boolean tlsAllowInvalidHostnames) {
        this.tlsAllowInvalidHostnames = tlsAllowInvalidHostnames;
    }

    public void setConnectTimeoutMS(Integer connectTimeoutMS) {
        this.connectTimeoutMS = connectTimeoutMS;
    }

    public void setSocketTimeoutMS(Integer socketTimeoutMS) {
        this.socketTimeoutMS = socketTimeoutMS;
    }

    public void setMaxIdleTimeMS(Integer maxIdleTimeMS) {
        this.maxIdleTimeMS = maxIdleTimeMS;
    }

    public void setMaxLifeTimeMS(Integer maxLifeTimeMS) {
        this.maxLifeTimeMS = maxLifeTimeMS;
    }

    public void setJournal(Boolean journal) {
        this.journal = journal;
    }

    public void setW(String w) {
        this.w = w;
    }

    public void setWtimeoutMS(Integer wtimeoutMS) {
        this.wtimeoutMS = wtimeoutMS;
    }

    public void setReadPreference(String readPreference) {
        this.readPreference = readPreference;
    }

    public void setReadPreferenceTags(String readPreferenceTags) {
        this.readPreferenceTags = readPreferenceTags;
    }

    public void setMaxStalenessSeconds(Integer maxStalenessSeconds) {
        this.maxStalenessSeconds = maxStalenessSeconds;
    }

    public void setAuthMechanism(String authMechanism) {
        this.authMechanism = authMechanism;
    }

    public void setAuthSource(String authSource) {
        this.authSource = authSource;
    }

    public void setAuthMechanismProperties(String authMechanismProperties) {
        this.authMechanismProperties = authMechanismProperties;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public void setCompressors(String compressors) {
        this.compressors = compressors;
    }

    public void setZlibCompressionLevel(Integer zlibCompressionLevel) {
        this.zlibCompressionLevel = zlibCompressionLevel;
    }

    public void setRetryWrites(Boolean retryWrites) {
        this.retryWrites = retryWrites;
    }

    public void setRetryReads(Boolean retryReads) {
        this.retryReads = retryReads;
    }

    public void setUuidRepresentation(String uuidRepresentation) {
        this.uuidRepresentation = uuidRepresentation;
    }

    public void setDirectConnection(Boolean directConnection) {
        this.directConnection = directConnection;
    }

    public void setMaxConnecting(Integer maxConnecting) {
        this.maxConnecting = maxConnecting;
    }

    public void setSrvServiceName(String srvServiceName) {
        this.srvServiceName = srvServiceName;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof BaseProperty)) {
            return false;
        } else {
            BaseProperty other = (BaseProperty)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                label479: {
                    Object this$minPoolSize = this.getMinPoolSize();
                    Object other$minPoolSize = other.getMinPoolSize();
                    if (this$minPoolSize == null) {
                        if (other$minPoolSize == null) {
                            break label479;
                        }
                    } else if (this$minPoolSize.equals(other$minPoolSize)) {
                        break label479;
                    }

                    return false;
                }

                Object this$maxPoolSize = this.getMaxPoolSize();
                Object other$maxPoolSize = other.getMaxPoolSize();
                if (this$maxPoolSize == null) {
                    if (other$maxPoolSize != null) {
                        return false;
                    }
                } else if (!this$maxPoolSize.equals(other$maxPoolSize)) {
                    return false;
                }

                Object this$waitQueueTimeoutMS = this.getWaitQueueTimeoutMS();
                Object other$waitQueueTimeoutMS = other.getWaitQueueTimeoutMS();
                if (this$waitQueueTimeoutMS == null) {
                    if (other$waitQueueTimeoutMS != null) {
                        return false;
                    }
                } else if (!this$waitQueueTimeoutMS.equals(other$waitQueueTimeoutMS)) {
                    return false;
                }

                label458: {
                    Object this$serverSelectionTimeoutMS = this.getServerSelectionTimeoutMS();
                    Object other$serverSelectionTimeoutMS = other.getServerSelectionTimeoutMS();
                    if (this$serverSelectionTimeoutMS == null) {
                        if (other$serverSelectionTimeoutMS == null) {
                            break label458;
                        }
                    } else if (this$serverSelectionTimeoutMS.equals(other$serverSelectionTimeoutMS)) {
                        break label458;
                    }

                    return false;
                }

                label451: {
                    Object this$localThresholdMS = this.getLocalThresholdMS();
                    Object other$localThresholdMS = other.getLocalThresholdMS();
                    if (this$localThresholdMS == null) {
                        if (other$localThresholdMS == null) {
                            break label451;
                        }
                    } else if (this$localThresholdMS.equals(other$localThresholdMS)) {
                        break label451;
                    }

                    return false;
                }

                Object this$heartbeatFrequencyMS = this.getHeartbeatFrequencyMS();
                Object other$heartbeatFrequencyMS = other.getHeartbeatFrequencyMS();
                if (this$heartbeatFrequencyMS == null) {
                    if (other$heartbeatFrequencyMS != null) {
                        return false;
                    }
                } else if (!this$heartbeatFrequencyMS.equals(other$heartbeatFrequencyMS)) {
                    return false;
                }

                Object this$ssl = this.getSsl();
                Object other$ssl = other.getSsl();
                if (this$ssl == null) {
                    if (other$ssl != null) {
                        return false;
                    }
                } else if (!this$ssl.equals(other$ssl)) {
                    return false;
                }

                label430: {
                    Object this$tls = this.getTls();
                    Object other$tls = other.getTls();
                    if (this$tls == null) {
                        if (other$tls == null) {
                            break label430;
                        }
                    } else if (this$tls.equals(other$tls)) {
                        break label430;
                    }

                    return false;
                }

                label423: {
                    Object this$tlsInsecure = this.getTlsInsecure();
                    Object other$tlsInsecure = other.getTlsInsecure();
                    if (this$tlsInsecure == null) {
                        if (other$tlsInsecure == null) {
                            break label423;
                        }
                    } else if (this$tlsInsecure.equals(other$tlsInsecure)) {
                        break label423;
                    }

                    return false;
                }

                Object this$tlsAllowInvalidHostnames = this.getTlsAllowInvalidHostnames();
                Object other$tlsAllowInvalidHostnames = other.getTlsAllowInvalidHostnames();
                if (this$tlsAllowInvalidHostnames == null) {
                    if (other$tlsAllowInvalidHostnames != null) {
                        return false;
                    }
                } else if (!this$tlsAllowInvalidHostnames.equals(other$tlsAllowInvalidHostnames)) {
                    return false;
                }

                label409: {
                    Object this$connectTimeoutMS = this.getConnectTimeoutMS();
                    Object other$connectTimeoutMS = other.getConnectTimeoutMS();
                    if (this$connectTimeoutMS == null) {
                        if (other$connectTimeoutMS == null) {
                            break label409;
                        }
                    } else if (this$connectTimeoutMS.equals(other$connectTimeoutMS)) {
                        break label409;
                    }

                    return false;
                }

                Object this$socketTimeoutMS = this.getSocketTimeoutMS();
                Object other$socketTimeoutMS = other.getSocketTimeoutMS();
                if (this$socketTimeoutMS == null) {
                    if (other$socketTimeoutMS != null) {
                        return false;
                    }
                } else if (!this$socketTimeoutMS.equals(other$socketTimeoutMS)) {
                    return false;
                }

                label395: {
                    Object this$maxIdleTimeMS = this.getMaxIdleTimeMS();
                    Object other$maxIdleTimeMS = other.getMaxIdleTimeMS();
                    if (this$maxIdleTimeMS == null) {
                        if (other$maxIdleTimeMS == null) {
                            break label395;
                        }
                    } else if (this$maxIdleTimeMS.equals(other$maxIdleTimeMS)) {
                        break label395;
                    }

                    return false;
                }

                Object this$maxLifeTimeMS = this.getMaxLifeTimeMS();
                Object other$maxLifeTimeMS = other.getMaxLifeTimeMS();
                if (this$maxLifeTimeMS == null) {
                    if (other$maxLifeTimeMS != null) {
                        return false;
                    }
                } else if (!this$maxLifeTimeMS.equals(other$maxLifeTimeMS)) {
                    return false;
                }

                Object this$journal = this.getJournal();
                Object other$journal = other.getJournal();
                if (this$journal == null) {
                    if (other$journal != null) {
                        return false;
                    }
                } else if (!this$journal.equals(other$journal)) {
                    return false;
                }

                label374: {
                    Object this$wtimeoutMS = this.getWtimeoutMS();
                    Object other$wtimeoutMS = other.getWtimeoutMS();
                    if (this$wtimeoutMS == null) {
                        if (other$wtimeoutMS == null) {
                            break label374;
                        }
                    } else if (this$wtimeoutMS.equals(other$wtimeoutMS)) {
                        break label374;
                    }

                    return false;
                }

                label367: {
                    Object this$maxStalenessSeconds = this.getMaxStalenessSeconds();
                    Object other$maxStalenessSeconds = other.getMaxStalenessSeconds();
                    if (this$maxStalenessSeconds == null) {
                        if (other$maxStalenessSeconds == null) {
                            break label367;
                        }
                    } else if (this$maxStalenessSeconds.equals(other$maxStalenessSeconds)) {
                        break label367;
                    }

                    return false;
                }

                Object this$zlibCompressionLevel = this.getZlibCompressionLevel();
                Object other$zlibCompressionLevel = other.getZlibCompressionLevel();
                if (this$zlibCompressionLevel == null) {
                    if (other$zlibCompressionLevel != null) {
                        return false;
                    }
                } else if (!this$zlibCompressionLevel.equals(other$zlibCompressionLevel)) {
                    return false;
                }

                Object this$retryWrites = this.getRetryWrites();
                Object other$retryWrites = other.getRetryWrites();
                if (this$retryWrites == null) {
                    if (other$retryWrites != null) {
                        return false;
                    }
                } else if (!this$retryWrites.equals(other$retryWrites)) {
                    return false;
                }

                label346: {
                    Object this$retryReads = this.getRetryReads();
                    Object other$retryReads = other.getRetryReads();
                    if (this$retryReads == null) {
                        if (other$retryReads == null) {
                            break label346;
                        }
                    } else if (this$retryReads.equals(other$retryReads)) {
                        break label346;
                    }

                    return false;
                }

                label339: {
                    Object this$directConnection = this.getDirectConnection();
                    Object other$directConnection = other.getDirectConnection();
                    if (this$directConnection == null) {
                        if (other$directConnection == null) {
                            break label339;
                        }
                    } else if (this$directConnection.equals(other$directConnection)) {
                        break label339;
                    }

                    return false;
                }

                Object this$maxConnecting = this.getMaxConnecting();
                Object other$maxConnecting = other.getMaxConnecting();
                if (this$maxConnecting == null) {
                    if (other$maxConnecting != null) {
                        return false;
                    }
                } else if (!this$maxConnecting.equals(other$maxConnecting)) {
                    return false;
                }

                Object this$host = this.getHost();
                Object other$host = other.getHost();
                if (this$host == null) {
                    if (other$host != null) {
                        return false;
                    }
                } else if (!this$host.equals(other$host)) {
                    return false;
                }

                label318: {
                    Object this$port = this.getPort();
                    Object other$port = other.getPort();
                    if (this$port == null) {
                        if (other$port == null) {
                            break label318;
                        }
                    } else if (this$port.equals(other$port)) {
                        break label318;
                    }

                    return false;
                }

                label311: {
                    Object this$database = this.getDatabase();
                    Object other$database = other.getDatabase();
                    if (this$database == null) {
                        if (other$database == null) {
                            break label311;
                        }
                    } else if (this$database.equals(other$database)) {
                        break label311;
                    }

                    return false;
                }

                Object this$username = this.getUsername();
                Object other$username = other.getUsername();
                if (this$username == null) {
                    if (other$username != null) {
                        return false;
                    }
                } else if (!this$username.equals(other$username)) {
                    return false;
                }

                label297: {
                    Object this$password = this.getPassword();
                    Object other$password = other.getPassword();
                    if (this$password == null) {
                        if (other$password == null) {
                            break label297;
                        }
                    } else if (this$password.equals(other$password)) {
                        break label297;
                    }

                    return false;
                }

                Object this$authenticationDatabase = this.getAuthenticationDatabase();
                Object other$authenticationDatabase = other.getAuthenticationDatabase();
                if (this$authenticationDatabase == null) {
                    if (other$authenticationDatabase != null) {
                        return false;
                    }
                } else if (!this$authenticationDatabase.equals(other$authenticationDatabase)) {
                    return false;
                }

                label283: {
                    Object this$replicaSet = this.getReplicaSet();
                    Object other$replicaSet = other.getReplicaSet();
                    if (this$replicaSet == null) {
                        if (other$replicaSet == null) {
                            break label283;
                        }
                    } else if (this$replicaSet.equals(other$replicaSet)) {
                        break label283;
                    }

                    return false;
                }

                Object this$w = this.getW();
                Object other$w = other.getW();
                if (this$w == null) {
                    if (other$w != null) {
                        return false;
                    }
                } else if (!this$w.equals(other$w)) {
                    return false;
                }

                Object this$readPreference = this.getReadPreference();
                Object other$readPreference = other.getReadPreference();
                if (this$readPreference == null) {
                    if (other$readPreference != null) {
                        return false;
                    }
                } else if (!this$readPreference.equals(other$readPreference)) {
                    return false;
                }

                label262: {
                    Object this$readPreferenceTags = this.getReadPreferenceTags();
                    Object other$readPreferenceTags = other.getReadPreferenceTags();
                    if (this$readPreferenceTags == null) {
                        if (other$readPreferenceTags == null) {
                            break label262;
                        }
                    } else if (this$readPreferenceTags.equals(other$readPreferenceTags)) {
                        break label262;
                    }

                    return false;
                }

                label255: {
                    Object this$authMechanism = this.getAuthMechanism();
                    Object other$authMechanism = other.getAuthMechanism();
                    if (this$authMechanism == null) {
                        if (other$authMechanism == null) {
                            break label255;
                        }
                    } else if (this$authMechanism.equals(other$authMechanism)) {
                        break label255;
                    }

                    return false;
                }

                Object this$authSource = this.getAuthSource();
                Object other$authSource = other.getAuthSource();
                if (this$authSource == null) {
                    if (other$authSource != null) {
                        return false;
                    }
                } else if (!this$authSource.equals(other$authSource)) {
                    return false;
                }

                Object this$authMechanismProperties = this.getAuthMechanismProperties();
                Object other$authMechanismProperties = other.getAuthMechanismProperties();
                if (this$authMechanismProperties == null) {
                    if (other$authMechanismProperties != null) {
                        return false;
                    }
                } else if (!this$authMechanismProperties.equals(other$authMechanismProperties)) {
                    return false;
                }

                label234: {
                    Object this$appName = this.getAppName();
                    Object other$appName = other.getAppName();
                    if (this$appName == null) {
                        if (other$appName == null) {
                            break label234;
                        }
                    } else if (this$appName.equals(other$appName)) {
                        break label234;
                    }

                    return false;
                }

                label227: {
                    Object this$compressors = this.getCompressors();
                    Object other$compressors = other.getCompressors();
                    if (this$compressors == null) {
                        if (other$compressors == null) {
                            break label227;
                        }
                    } else if (this$compressors.equals(other$compressors)) {
                        break label227;
                    }

                    return false;
                }

                Object this$uuidRepresentation = this.getUuidRepresentation();
                Object other$uuidRepresentation = other.getUuidRepresentation();
                if (this$uuidRepresentation == null) {
                    if (other$uuidRepresentation != null) {
                        return false;
                    }
                } else if (!this$uuidRepresentation.equals(other$uuidRepresentation)) {
                    return false;
                }

                Object this$srvServiceName = this.getSrvServiceName();
                Object other$srvServiceName = other.getSrvServiceName();
                if (this$srvServiceName == null) {
                    if (other$srvServiceName != null) {
                        return false;
                    }
                } else if (!this$srvServiceName.equals(other$srvServiceName)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof BaseProperty;
    }

    public int hashCode() {
        int result = 1;
        Object $minPoolSize = this.getMinPoolSize();
        result = result * 59 + ($minPoolSize == null ? 43 : $minPoolSize.hashCode());
        Object $maxPoolSize = this.getMaxPoolSize();
        result = result * 59 + ($maxPoolSize == null ? 43 : $maxPoolSize.hashCode());
        Object $waitQueueTimeoutMS = this.getWaitQueueTimeoutMS();
        result = result * 59 + ($waitQueueTimeoutMS == null ? 43 : $waitQueueTimeoutMS.hashCode());
        Object $serverSelectionTimeoutMS = this.getServerSelectionTimeoutMS();
        result = result * 59 + ($serverSelectionTimeoutMS == null ? 43 : $serverSelectionTimeoutMS.hashCode());
        Object $localThresholdMS = this.getLocalThresholdMS();
        result = result * 59 + ($localThresholdMS == null ? 43 : $localThresholdMS.hashCode());
        Object $heartbeatFrequencyMS = this.getHeartbeatFrequencyMS();
        result = result * 59 + ($heartbeatFrequencyMS == null ? 43 : $heartbeatFrequencyMS.hashCode());
        Object $ssl = this.getSsl();
        result = result * 59 + ($ssl == null ? 43 : $ssl.hashCode());
        Object $tls = this.getTls();
        result = result * 59 + ($tls == null ? 43 : $tls.hashCode());
        Object $tlsInsecure = this.getTlsInsecure();
        result = result * 59 + ($tlsInsecure == null ? 43 : $tlsInsecure.hashCode());
        Object $tlsAllowInvalidHostnames = this.getTlsAllowInvalidHostnames();
        result = result * 59 + ($tlsAllowInvalidHostnames == null ? 43 : $tlsAllowInvalidHostnames.hashCode());
        Object $connectTimeoutMS = this.getConnectTimeoutMS();
        result = result * 59 + ($connectTimeoutMS == null ? 43 : $connectTimeoutMS.hashCode());
        Object $socketTimeoutMS = this.getSocketTimeoutMS();
        result = result * 59 + ($socketTimeoutMS == null ? 43 : $socketTimeoutMS.hashCode());
        Object $maxIdleTimeMS = this.getMaxIdleTimeMS();
        result = result * 59 + ($maxIdleTimeMS == null ? 43 : $maxIdleTimeMS.hashCode());
        Object $maxLifeTimeMS = this.getMaxLifeTimeMS();
        result = result * 59 + ($maxLifeTimeMS == null ? 43 : $maxLifeTimeMS.hashCode());
        Object $journal = this.getJournal();
        result = result * 59 + ($journal == null ? 43 : $journal.hashCode());
        Object $wtimeoutMS = this.getWtimeoutMS();
        result = result * 59 + ($wtimeoutMS == null ? 43 : $wtimeoutMS.hashCode());
        Object $maxStalenessSeconds = this.getMaxStalenessSeconds();
        result = result * 59 + ($maxStalenessSeconds == null ? 43 : $maxStalenessSeconds.hashCode());
        Object $zlibCompressionLevel = this.getZlibCompressionLevel();
        result = result * 59 + ($zlibCompressionLevel == null ? 43 : $zlibCompressionLevel.hashCode());
        Object $retryWrites = this.getRetryWrites();
        result = result * 59 + ($retryWrites == null ? 43 : $retryWrites.hashCode());
        Object $retryReads = this.getRetryReads();
        result = result * 59 + ($retryReads == null ? 43 : $retryReads.hashCode());
        Object $directConnection = this.getDirectConnection();
        result = result * 59 + ($directConnection == null ? 43 : $directConnection.hashCode());
        Object $maxConnecting = this.getMaxConnecting();
        result = result * 59 + ($maxConnecting == null ? 43 : $maxConnecting.hashCode());
        Object $host = this.getHost();
        result = result * 59 + ($host == null ? 43 : $host.hashCode());
        Object $port = this.getPort();
        result = result * 59 + ($port == null ? 43 : $port.hashCode());
        Object $database = this.getDatabase();
        result = result * 59 + ($database == null ? 43 : $database.hashCode());
        Object $username = this.getUsername();
        result = result * 59 + ($username == null ? 43 : $username.hashCode());
        Object $password = this.getPassword();
        result = result * 59 + ($password == null ? 43 : $password.hashCode());
        Object $authenticationDatabase = this.getAuthenticationDatabase();
        result = result * 59 + ($authenticationDatabase == null ? 43 : $authenticationDatabase.hashCode());
        Object $replicaSet = this.getReplicaSet();
        result = result * 59 + ($replicaSet == null ? 43 : $replicaSet.hashCode());
        Object $w = this.getW();
        result = result * 59 + ($w == null ? 43 : $w.hashCode());
        Object $readPreference = this.getReadPreference();
        result = result * 59 + ($readPreference == null ? 43 : $readPreference.hashCode());
        Object $readPreferenceTags = this.getReadPreferenceTags();
        result = result * 59 + ($readPreferenceTags == null ? 43 : $readPreferenceTags.hashCode());
        Object $authMechanism = this.getAuthMechanism();
        result = result * 59 + ($authMechanism == null ? 43 : $authMechanism.hashCode());
        Object $authSource = this.getAuthSource();
        result = result * 59 + ($authSource == null ? 43 : $authSource.hashCode());
        Object $authMechanismProperties = this.getAuthMechanismProperties();
        result = result * 59 + ($authMechanismProperties == null ? 43 : $authMechanismProperties.hashCode());
        Object $appName = this.getAppName();
        result = result * 59 + ($appName == null ? 43 : $appName.hashCode());
        Object $compressors = this.getCompressors();
        result = result * 59 + ($compressors == null ? 43 : $compressors.hashCode());
        Object $uuidRepresentation = this.getUuidRepresentation();
        result = result * 59 + ($uuidRepresentation == null ? 43 : $uuidRepresentation.hashCode());
        Object $srvServiceName = this.getSrvServiceName();
        result = result * 59 + ($srvServiceName == null ? 43 : $srvServiceName.hashCode());
        return result;
    }

    public String toString() {
        return "BaseProperty(host=" + this.getHost() + ", port=" + this.getPort() + ", database=" + this.getDatabase() + ", username=" + this.getUsername() + ", password=" + this.getPassword() + ", authenticationDatabase=" + this.getAuthenticationDatabase() + ", minPoolSize=" + this.getMinPoolSize() + ", maxPoolSize=" + this.getMaxPoolSize() + ", waitQueueTimeoutMS=" + this.getWaitQueueTimeoutMS() + ", serverSelectionTimeoutMS=" + this.getServerSelectionTimeoutMS() + ", localThresholdMS=" + this.getLocalThresholdMS() + ", heartbeatFrequencyMS=" + this.getHeartbeatFrequencyMS() + ", replicaSet=" + this.getReplicaSet() + ", ssl=" + this.getSsl() + ", tls=" + this.getTls() + ", tlsInsecure=" + this.getTlsInsecure() + ", tlsAllowInvalidHostnames=" + this.getTlsAllowInvalidHostnames() + ", connectTimeoutMS=" + this.getConnectTimeoutMS() + ", socketTimeoutMS=" + this.getSocketTimeoutMS() + ", maxIdleTimeMS=" + this.getMaxIdleTimeMS() + ", maxLifeTimeMS=" + this.getMaxLifeTimeMS() + ", journal=" + this.getJournal() + ", w=" + this.getW() + ", wtimeoutMS=" + this.getWtimeoutMS() + ", readPreference=" + this.getReadPreference() + ", readPreferenceTags=" + this.getReadPreferenceTags() + ", maxStalenessSeconds=" + this.getMaxStalenessSeconds() + ", authMechanism=" + this.getAuthMechanism() + ", authSource=" + this.getAuthSource() + ", authMechanismProperties=" + this.getAuthMechanismProperties() + ", appName=" + this.getAppName() + ", compressors=" + this.getCompressors() + ", zlibCompressionLevel=" + this.getZlibCompressionLevel() + ", retryWrites=" + this.getRetryWrites() + ", retryReads=" + this.getRetryReads() + ", uuidRepresentation=" + this.getUuidRepresentation() + ", directConnection=" + this.getDirectConnection() + ", maxConnecting=" + this.getMaxConnecting() + ", srvServiceName=" + this.getSrvServiceName() + ")";
    }

    public BaseProperty(String host, String port, String database, String username, String password, String authenticationDatabase, Integer minPoolSize, Integer maxPoolSize, Integer waitQueueTimeoutMS, Integer serverSelectionTimeoutMS, Integer localThresholdMS, Integer heartbeatFrequencyMS, String replicaSet, Boolean ssl, Boolean tls, Boolean tlsInsecure, Boolean tlsAllowInvalidHostnames, Integer connectTimeoutMS, Integer socketTimeoutMS, Integer maxIdleTimeMS, Integer maxLifeTimeMS, Boolean journal, String w, Integer wtimeoutMS, String readPreference, String readPreferenceTags, Integer maxStalenessSeconds, String authMechanism, String authSource, String authMechanismProperties, String appName, String compressors, Integer zlibCompressionLevel, Boolean retryWrites, Boolean retryReads, String uuidRepresentation, Boolean directConnection, Integer maxConnecting, String srvServiceName) {
        this.host = host;
        this.port = port;
        this.database = database;
        this.username = username;
        this.password = password;
        this.authenticationDatabase = authenticationDatabase;
        this.minPoolSize = minPoolSize;
        this.maxPoolSize = maxPoolSize;
        this.waitQueueTimeoutMS = waitQueueTimeoutMS;
        this.serverSelectionTimeoutMS = serverSelectionTimeoutMS;
        this.localThresholdMS = localThresholdMS;
        this.heartbeatFrequencyMS = heartbeatFrequencyMS;
        this.replicaSet = replicaSet;
        this.ssl = ssl;
        this.tls = tls;
        this.tlsInsecure = tlsInsecure;
        this.tlsAllowInvalidHostnames = tlsAllowInvalidHostnames;
        this.connectTimeoutMS = connectTimeoutMS;
        this.socketTimeoutMS = socketTimeoutMS;
        this.maxIdleTimeMS = maxIdleTimeMS;
        this.maxLifeTimeMS = maxLifeTimeMS;
        this.journal = journal;
        this.w = w;
        this.wtimeoutMS = wtimeoutMS;
        this.readPreference = readPreference;
        this.readPreferenceTags = readPreferenceTags;
        this.maxStalenessSeconds = maxStalenessSeconds;
        this.authMechanism = authMechanism;
        this.authSource = authSource;
        this.authMechanismProperties = authMechanismProperties;
        this.appName = appName;
        this.compressors = compressors;
        this.zlibCompressionLevel = zlibCompressionLevel;
        this.retryWrites = retryWrites;
        this.retryReads = retryReads;
        this.uuidRepresentation = uuidRepresentation;
        this.directConnection = directConnection;
        this.maxConnecting = maxConnecting;
        this.srvServiceName = srvServiceName;
    }

    public BaseProperty() {
    }
}
