package com.anwen.mongo.encryptor;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.StringUtils;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.util.encoders.Hex;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.security.Security;

/**
 * SM4加密
 *
 * @author anwen
 * @date 2024/6/30 上午1:25
 */
public class SM4Example implements Encryptor {

    static {
        Security.addProvider(new BouncyCastleProvider());
    }

    @Override
    public String encrypt(String data, String key,String publicKey) throws Exception {
        if (StringUtils.isBlank(key)){
            key = PropertyCache.publicKey;
        }
        byte[] keyBytes = Hex.decode(key);
        SecretKeySpec secretKeySpec = new SecretKeySpec(keyBytes, "SM4");
        Cipher cipher = Cipher.getInstance("SM4/ECB/PKCS5Padding", "BC");
        cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec);
        byte[] encryptedBytes = cipher.doFinal(data.getBytes());
        return Hex.toHexString(encryptedBytes);
    }

    @Override
    public String decrypt(String data, String key, String privateKey) throws Exception {
        if (StringUtils.isBlank(key)){
            key = PropertyCache.publicKey;
        }
        byte[] keyBytes = Hex.decode(key);
        SecretKeySpec secretKeySpec = new SecretKeySpec(keyBytes, "SM4");
        Cipher cipher = Cipher.getInstance("SM4/ECB/PKCS5Padding", "BC");
        cipher.init(Cipher.DECRYPT_MODE, secretKeySpec);
        byte[] encryptedBytes = Hex.decode(data);
        return new String(cipher.doFinal(encryptedBytes));
    }

}
