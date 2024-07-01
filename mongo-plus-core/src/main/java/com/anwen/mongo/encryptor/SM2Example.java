package com.anwen.mongo.encryptor;

import com.anwen.mongo.cache.global.PropertyCache;
import com.anwen.mongo.toolkit.EncryptorUtil;
import com.anwen.mongo.toolkit.StringUtils;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.util.encoders.Hex;

import javax.crypto.Cipher;
import java.security.*;
import java.security.spec.ECGenParameterSpec;

/**
 * SM2加密
 *
 * @author anwen
 * @date 2024/6/30 上午12:45
 */
public class SM2Example implements Encryptor {

    static {
        Security.addProvider(new BouncyCastleProvider());
    }

    private final String ALGORITHM = "EC";

    /**
     * 生成SM2密钥对
     * @return {@link java.security.KeyPair}
     * @author anwen
     * @date 2024/6/30 上午1:23
     */
    public KeyPair generateKeyPair() throws Exception {
        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("EC", "BC");
        keyPairGenerator.initialize(new ECGenParameterSpec("sm2p256v1"));
        return keyPairGenerator.generateKeyPair();
    }

    /**
     * 使用SM2公钥加密
     * @author anwen
     * @date 2024/6/30 上午1:23
     */
    public String encrypt(String data,PublicKey publicKey) throws Exception {
        Cipher cipher = Cipher.getInstance("SM2", BouncyCastleProvider.PROVIDER_NAME);
        cipher.init(Cipher.ENCRYPT_MODE, publicKey);
        return Hex.toHexString(cipher.doFinal(data.getBytes()));
    }

    @Override
    public String encrypt(String data,String key,String publicKey) throws Exception {
        if (StringUtils.isBlank(publicKey)){
            publicKey = PropertyCache.publicKey;
        }
        return encrypt(data,EncryptorUtil.getPublicKeyFromString(publicKey,ALGORITHM,BouncyCastleProvider.PROVIDER_NAME));
    }
    
    /**
     * 使用SM2私钥解密
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/30 上午1:23
     */
    public String decrypt(String data,PrivateKey privateKey) throws Exception {
        Cipher cipher = Cipher.getInstance("SM2", BouncyCastleProvider.PROVIDER_NAME);
        cipher.init(Cipher.DECRYPT_MODE, privateKey);
        return new String(cipher.doFinal(Hex.decode(data)));
    }

    @Override
    public String decrypt(String data, String key, String privateKey) throws Exception {
        if (StringUtils.isBlank(privateKey)){
            privateKey = PropertyCache.publicKey;
        }
        return decrypt(data,EncryptorUtil.getPrivateKeyFromString(privateKey,ALGORITHM,BouncyCastleProvider.PROVIDER_NAME));
    }
}
