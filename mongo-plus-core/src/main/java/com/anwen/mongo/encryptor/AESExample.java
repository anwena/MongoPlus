package com.anwen.mongo.encryptor;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.StringUtils;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import static com.anwen.mongo.toolkit.StringUtils.hexToBytes;

/**
 * AES对称加密
 *
 * @author anwen
 * @date 2024/6/29 下午1:25
 */
public class AESExample implements Encryptor {

    private final String ALGORITHM = "AES";

    /**
     * AES加密
     * @param data 密文
     * @param password 密码
     * @return {@link byte[]}
     * @author anwen
     * @date 2024/6/29 下午1:26
     */
    @Override
    public String encrypt(String data, String password,String publicKey) throws Exception {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        if (StringUtils.isBlank(password)){
            password = PropertyCache.key;
        }
        cipher.init(Cipher.ENCRYPT_MODE, getKeyFromPassword(password));
        return StringUtils.bytesToHex(cipher.doFinal(data.getBytes()));
    }

    /**
     * AES解密
     *
     * @param encryptedData 密文
     * @param password      密码
     * @param privateKey 私钥
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/29 下午1:27
     */
    @Override
    public String decrypt(String encryptedData, String password, String privateKey) throws Exception {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        if (StringUtils.isBlank(password)){
            password = PropertyCache.key;
        }
        cipher.init(Cipher.DECRYPT_MODE, getKeyFromPassword(password));
        return new String(cipher.doFinal(hexToBytes(encryptedData)));
    }

    /**
     * 生成AES密钥
     * @return {@link javax.crypto.SecretKey}
     * @author anwen
     * @date 2024/6/29 下午1:27
     */
    public SecretKey getKeyFromPassword(String password) throws NoSuchAlgorithmException {
        byte[] key = password.getBytes();
        MessageDigest sha = MessageDigest.getInstance("SHA-256");
        key = sha.digest(key);
        // 使用前16字节生成AES密钥
        key = Arrays.copyOf(key, 16);
        return new SecretKeySpec(key, ALGORITHM);
    }

}
