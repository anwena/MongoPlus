package com.anwen.mongo.toolkit;

import com.anwen.mongo.cache.global.DataSourceNameCache;
import com.anwen.mongo.listener.BaseListener;
import com.anwen.mongo.model.BaseProperty;
import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.connection.SslSettings;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.*;
import java.security.cert.CertificateException;
import java.util.Collections;

/**
 * @author anwen
 * @date 2024/7/9 下午4:20
 */
public class MongoUtil {

    /**
     * 建立连接
     * @param dsName 数据源名称
     * @param baseProperty 配置
     * @return {@link MongoClient}
     * @author anwen
     * @date 2024/5/27 下午11:20
     */
    public static MongoClient getMongo(String dsName, BaseProperty baseProperty){
        SslSettings sslSettings = null;
        if (baseProperty.getSsl()){
            try {
                // 加载客户端密钥库
                KeyStore clientKeyStore = KeyStore.getInstance("JKS");
                clientKeyStore.load(Files.newInputStream(Paths.get(baseProperty.getClientKeyStore())), baseProperty.getKeyPassword().toCharArray());

                // 初始化KeyManager
                KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
                keyManagerFactory.init(clientKeyStore, baseProperty.getKeyPassword().toCharArray());
                TrustManager[] getTrustManagers = null;
                if (StringUtils.isNotBlank(baseProperty.getJks())) {
                    // 加载信任库
                    KeyStore trustStore = KeyStore.getInstance("JKS");
                    trustStore.load(Files.newInputStream(Paths.get(baseProperty.getJks())), baseProperty.getKeyPassword().toCharArray());
                    // 初始化TrustManager
                    TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance("SunX509");
                    trustManagerFactory.init(trustStore);
                    getTrustManagers = trustManagerFactory.getTrustManagers();
                }
                // 初始化SSL上下文
                SSLContext sslContext = SSLContext.getInstance("TLS");
                sslContext.init(keyManagerFactory.getKeyManagers(), getTrustManagers, null);
                // 配置MongoClientOptions以使用SSL
                sslSettings = SslSettings.builder()
                        .enabled(true)
                        .invalidHostNameAllowed(baseProperty.isInvalidHostNameAllowed())
                        .context(sslContext)
                        .build();
            } catch (NoSuchAlgorithmException | KeyManagementException | CertificateException | KeyStoreException |
                     IOException | UnrecoverableKeyException e) {
                throw new RuntimeException(e);
            }
        }
        return getMongo(dsName,baseProperty,sslSettings);
    }

    public static MongoClient getMongo(String dsName,BaseProperty baseProperty,SslSettings sslSettings){
        DataSourceNameCache.setBaseProperty(dsName,baseProperty);
        MongoClientSettings.Builder builder = MongoClientSettings.builder();
        if (sslSettings != null){
            builder.applyToSslSettings(ssl -> ssl.applySettings(sslSettings));
        }
        builder.applyConnectionString(new ConnectionString(new UrlJoint(baseProperty).jointMongoUrl())).commandListenerList(Collections.singletonList(new BaseListener()));
        return MongoClients.create(builder.build());
    }

}
