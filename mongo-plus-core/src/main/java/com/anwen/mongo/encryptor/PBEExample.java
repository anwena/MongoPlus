package com.anwen.mongo.encryptor;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.StringUtils;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import java.security.SecureRandom;

import static com.anwen.mongo.toolkit.StringUtils.bytesToHex;
import static com.anwen.mongo.toolkit.StringUtils.hexToBytes;

/**
 * PEB混合算法
 *
 * @author anwen
 * @date 2024/6/30 下午12:27
 */
public class PBEExample implements Encryptor {

    private final String algorithm;

    public PBEExample(String algorithm) {
        this.algorithm = algorithm;
    }

    private static final int ITERATION_COUNT = 1000;

    // 生成随机盐
    private static byte[] generateSalt() {
        byte[] salt = new byte[8];
        SecureRandom random = new SecureRandom();
        random.nextBytes(salt);
        return salt;
    }

    @Override
    public String encrypt(String data, String key,String publicKey) throws Exception {
        byte[] salt = generateSalt();
        PBEParameterSpec pbeParamSpec = new PBEParameterSpec(salt, ITERATION_COUNT);
        if (StringUtils.isBlank(key)){
            key = PropertyCache.key;
        }
        PBEKeySpec pbeKeySpec = new PBEKeySpec(key.toCharArray());
        SecretKeyFactory keyFac = SecretKeyFactory.getInstance(algorithm);
        SecretKey pbeKey = keyFac.generateSecret(pbeKeySpec);
        Cipher cipher = Cipher.getInstance(algorithm);
        cipher.init(Cipher.ENCRYPT_MODE, pbeKey, pbeParamSpec);
        byte[] encryptedBytes = cipher.doFinal(data.getBytes());

        // 将盐和加密数据一起返回，转换为十六进制字符串
        byte[] encryptedWithSalt = new byte[salt.length + encryptedBytes.length];
        System.arraycopy(salt, 0, encryptedWithSalt, 0, salt.length);
        System.arraycopy(encryptedBytes, 0, encryptedWithSalt, salt.length, encryptedBytes.length);

        return bytesToHex(encryptedWithSalt);
    }

    @Override
    public String decrypt(String data, String key, String privateKey) throws Exception {
        byte[] encryptedWithSalt = hexToBytes(data);
        byte[] salt = new byte[8];
        byte[] encryptedBytes = new byte[encryptedWithSalt.length - salt.length];

        System.arraycopy(encryptedWithSalt, 0, salt, 0, salt.length);
        System.arraycopy(encryptedWithSalt, salt.length, encryptedBytes, 0, encryptedBytes.length);

        PBEParameterSpec pbeParamSpec = new PBEParameterSpec(salt, ITERATION_COUNT);
        PBEKeySpec pbeKeySpec = new PBEKeySpec(key.toCharArray());
        SecretKeyFactory keyFac = SecretKeyFactory.getInstance(algorithm);
        SecretKey pbeKey = keyFac.generateSecret(pbeKeySpec);
        Cipher cipher = Cipher.getInstance(algorithm);
        cipher.init(Cipher.DECRYPT_MODE, pbeKey, pbeParamSpec);
        byte[] decryptedBytes = cipher.doFinal(encryptedBytes);
        return new String(decryptedBytes);
    }

}
