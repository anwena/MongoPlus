package com.anwen.mongo.encryptor;

import com.anwen.mongo.toolkit.StringUtils;
import org.bouncycastle.jce.provider.BouncyCastleProvider;

import java.security.MessageDigest;
import java.security.Security;

/**
 * SM3加密
 *
 * @author anwen
 * @date 2024/6/30 上午1:12
 */
public class SM3Example implements Encryptor {

    static {
        Security.addProvider(new BouncyCastleProvider());
    }

    @Override
    public String encrypt(String data, String key,String publicKey) throws Exception {
        return StringUtils.bytesToHex(MessageDigest.getInstance("SM3", BouncyCastleProvider.PROVIDER_NAME).digest(data.getBytes()));
    }

    @Override
    public String decrypt(String data, String key, String privateKey) throws Exception {
        return data;
    }
}
