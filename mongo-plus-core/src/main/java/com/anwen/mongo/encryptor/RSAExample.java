package com.anwen.mongo.encryptor;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.StringUtils;

import javax.crypto.Cipher;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;

import static com.anwen.mongo.toolkit.EncryptorUtil.getPrivateKeyFromString;
import static com.anwen.mongo.toolkit.EncryptorUtil.getPublicKeyFromString;

/**
 * RSA非对称加密
 *
 * @author anwen
 * @date 2024/6/30 上午12:16
 */
public class RSAExample implements Encryptor {

    private final String ALGORITHM = "RSA";

    /**
     * RSA加密
     * @param data 明文
     * @param publicKey 公钥
     * @return {@link byte[]}
     * @author anwen
     * @date 2024/6/30 上午12:17
     */
    public String encrypt(String data, PublicKey publicKey) throws Exception {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, publicKey);
        return StringUtils.bytesToHex(cipher.doFinal(data.getBytes()));
    }

    @Override
    public String encrypt(String data, String key,String publicKey) throws Exception {
        if (StringUtils.isBlank(publicKey)){
            publicKey = PropertyCache.publicKey;
        }
        return encrypt(data,getPublicKeyFromString(publicKey,ALGORITHM));
    }

    /**
     * RSA解密
     * @param encryptedData 密文
     * @param privateKey 私钥
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/30 上午12:17
     */
    public String decrypt(String encryptedData, PrivateKey privateKey) throws Exception {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        cipher.init(Cipher.DECRYPT_MODE, privateKey);
        return new String(cipher.doFinal(StringUtils.hexToBytes(encryptedData)));
    }

    @Override
    public String decrypt(String encryptedData, String key, String privateKey) throws Exception {
        if (StringUtils.isBlank(privateKey)){
            privateKey = PropertyCache.publicKey;
        }
        return decrypt(encryptedData,getPrivateKeyFromString(privateKey,ALGORITHM));
    }

    /**
     * 生成RSA密钥对
     * @return {@link java.security.KeyPair}
     * @author anwen
     * @date 2024/6/30 上午12:17
     */
    public KeyPair generateKeyPair() throws Exception {
        KeyPairGenerator keyGen = KeyPairGenerator.getInstance(ALGORITHM);
        keyGen.initialize(2048); // 可选：1024, 2048, 4096
        return keyGen.generateKeyPair();
    }
}
