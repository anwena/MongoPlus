package com.anwen.mongo.toolkit;

import com.anwen.mongo.domain.InitMongoCollectionException;
import com.anwen.mongo.model.BaseProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * @Description: 拼接mongodb连接
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.utils
 * @Author: JiaChaoYang
 * @CreateTime: 2023-02-26 21:56
 * @Version: 1.0
 */
public class UrlJoint {

    private static final Logger logger = LoggerFactory.getLogger(UrlJoint.class);

    private final BaseProperty baseProperty;

    public UrlJoint(BaseProperty baseProperty){
        this.baseProperty = baseProperty;
    }

    StringBuilder uri = new StringBuilder("mongodb://");

    public String jointMongoUrl(){
        if (StringUtils.isNotBlank(baseProperty.getUsername()) && StringUtils.isNotBlank(baseProperty.getPassword())){
            try {
                baseProperty.setUsername(URLEncoder.encode(baseProperty.getUsername(), "UTF-8"));
                baseProperty.setPassword(URLEncoder.encode(baseProperty.getPassword(),"UTF-8"));
            } catch (UnsupportedEncodingException e) {
                logger.error(e.getMessage());
                throw new RuntimeException(e);
            }
            uri.append(baseProperty.getUsername()).append(":").append(baseProperty.getPassword()).append("@");
        }
        if (StringUtils.isNotBlank(baseProperty.getHost()) && StringUtils.isNotBlank(baseProperty.getPort())){
            if (baseProperty.getHost().contains(",")){
                String[] hostArray = baseProperty.getHost().split(",");
                String[] portArray = baseProperty.getPort().split(",");
                if (hostArray.length != portArray.length && portArray.length > 1){
                    throw new InitMongoCollectionException("Host and port do not match");
                }
                for (int i = 0; i < hostArray.length; i++) {
                    String host = hostArray[i];
                    uri.append(host).append(":").append(portArray.length > 1 ? portArray[i] : portArray[0]).append(",");
                }
                uri.deleteCharAt(uri.length()-1);
            }else {
                uri.append(baseProperty.getHost()).append(":").append(baseProperty.getPort());
            }
            uri.append("/");
            if (StringUtils.isNotBlank(baseProperty.getAuthenticationDatabase())){
                uri.append(baseProperty.getAuthenticationDatabase());
            }
        }
        if (baseProperty.getMinPoolSize() != null){
            uri.append("?minPoolSize=").append(baseProperty.getMinPoolSize());
        }
        if (baseProperty.getMaxPoolSize() != null){
            uri.append("&maxPoolSize=").append(baseProperty.getMaxPoolSize());
        }
        if (baseProperty.getWaitQueueTimeoutMS() != null){
            uri.append("&waitQueueTimeoutMS=").append(baseProperty.getWaitQueueTimeoutMS());
        }
        if (baseProperty.getServerSelectionTimeoutMS() != null){
            uri.append("&serverSelectionTimeoutMS=").append(baseProperty.getServerSelectionTimeoutMS());
        }
        if (baseProperty.getLocalThresholdMS() != null){
            uri.append("&localThresholdMS=").append(baseProperty.getLocalThresholdMS());
        }
        if (baseProperty.getHeartbeatFrequencyMS() != null){
            uri.append("&heartbeatFrequencyMS=").append(baseProperty.getHeartbeatFrequencyMS());
        }
        if (baseProperty.getReplicaSet() != null){
            uri.append("&replicaSet=").append(baseProperty.getReplicaSet());
        }
        if (baseProperty.getSsl() != null){
            uri.append("&ssl=").append(baseProperty.getSsl());
        }
        if (baseProperty.getTls() != null){
            uri.append("&tls=").append(baseProperty.getTls());
        }
        if (baseProperty.getTlsInsecure() != null){
            uri.append("&tlsInsecure=").append(baseProperty.getTlsInsecure());
        }
        if (baseProperty.getTlsAllowInvalidHostnames() != null){
            uri.append("&tlsAllowInvalidHostnames=").append(baseProperty.getTlsAllowInvalidHostnames());
        }
        if (baseProperty.getConnectTimeoutMS() != null){
            uri.append("&connectTimeoutMS=").append(baseProperty.getConnectTimeoutMS());
        }
        if (baseProperty.getSocketTimeoutMS() != null){
            uri.append("&socketTimeoutMS=").append(baseProperty.getSocketTimeoutMS());
        }
        if (baseProperty.getMaxIdleTimeMS() != null){
            uri.append("&maxIdleTimeMS=").append(baseProperty.getMaxIdleTimeMS());
        }
        if (baseProperty.getMaxLifeTimeMS() != null){
            uri.append("&maxLifeTimeMS=").append(baseProperty.getMaxLifeTimeMS());
        }
        if (baseProperty.getJournal() != null){
            uri.append("&journal=").append(baseProperty.getJournal());
        }
        if (baseProperty.getW() != null){
            uri.append("&w=").append(baseProperty.getW());
        }
        if (baseProperty.getWtimeoutMS() != null){
            uri.append("&wtimeoutMS=").append(baseProperty.getWtimeoutMS());
        }
        if (baseProperty.getReadPreference() != null){
            uri.append("&readPreference=").append(baseProperty.getReadPreference());
        }
        if (baseProperty.getReadPreferenceTags() != null){
            uri.append("&readPreferenceTags=").append(baseProperty.getReadPreferenceTags());
        }
        if (baseProperty.getMaxStalenessSeconds() != null){
            uri.append("&maxStalenessSeconds=").append(baseProperty.getMaxStalenessSeconds());
        }
        if (baseProperty.getAuthMechanism() != null){
            uri.append("&authMechanism=").append(baseProperty.getAuthMechanism());
        }
        if (baseProperty.getAuthSource() != null){
            uri.append("&authSource=").append(baseProperty.getAuthSource());
        }
        if (baseProperty.getAuthMechanismProperties() != null){
            uri.append("&authMechanismProperties=").append(baseProperty.getAuthMechanismProperties());
        }
        if (baseProperty.getAppName() != null){
            uri.append("&appName=").append(baseProperty.getAppName());
        }
        if (baseProperty.getCompressors() != null){
            uri.append("&compressors=").append(baseProperty.getCompressors());
        }
        if (baseProperty.getZlibCompressionLevel() != null){
            uri.append("&zlibCompressionLevel=").append(baseProperty.getZlibCompressionLevel());
        }
        if (baseProperty.getRetryWrites() != null){
            uri.append("&retryWrites=").append(baseProperty.getRetryWrites());
        }
        if (baseProperty.getRetryReads() != null){
            uri.append("&retryReads=").append(baseProperty.getRetryReads());
        }
        if (baseProperty.getUuidRepresentation() != null){
            uri.append("&uuidRepresentation=").append(baseProperty.getUuidRepresentation());
        }
        if (baseProperty.getDirectConnection() != null){
            uri.append("&directConnection=").append(baseProperty.getDirectConnection());
        }
        if (baseProperty.getMaxConnecting() != null){
            uri.append("&maxConnecting=").append(baseProperty.getMaxConnecting());
        }
        if (baseProperty.getSrvServiceName() != null){
            uri.append("&srvServiceName=").append(baseProperty.getSrvServiceName());
        }
        String mongodbUrl = uri.toString();
        if (!mongodbUrl.contains("?")){
            mongodbUrl = mongodbUrl.replaceFirst("&", "?");
        }
        logger.info("get connected：{}",mongodbUrl);
        return mongodbUrl;
    }

}
