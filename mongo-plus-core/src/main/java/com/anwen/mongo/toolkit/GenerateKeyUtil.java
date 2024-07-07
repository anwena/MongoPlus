package com.anwen.mongo.toolkit;

import com.anwen.mongo.model.MutablePair;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.util.encoders.Hex;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.security.*;
import java.security.spec.ECGenParameterSpec;
import java.util.Arrays;

/**
 * 生成秘钥
 *
 * @author anwen
 * @date 2024/7/1 下午3:41
 */
public class GenerateKeyUtil {

    /**
     * 生成AES对称加密秘钥
     * @return {@link javax.crypto.SecretKey}
     * @author anwen
     * @date 2024/6/29 下午1:27
     */
    public static String generateAESKey(String password) throws NoSuchAlgorithmException {
        byte[] key = password.getBytes();
        MessageDigest sha = MessageDigest.getInstance("SHA-256");
        key = sha.digest(key);
        // 使用前16字节生成AES密钥
        key = Arrays.copyOf(key, 16);
        SecretKeySpec secretKeySpec = new SecretKeySpec(key, "AES");
        return StringUtils.bytesToHex(secretKeySpec.getEncoded());
    }

    /**
     * 生成RSA密钥对，返回参数left=公钥，right=私钥
     * @return {@link com.anwen.mongo.model.MutablePair<java.lang.String,java.lang.String>}
     * @author anwen
     * @date 2024/7/1 下午3:44
     */
    public static MutablePair<String,String> generateRSAKey() throws Exception {
        KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA");
        keyGen.initialize(2048); // 可选：1024, 2048, 4096
        KeyPair keyPair = keyGen.generateKeyPair();
        PrivateKey privateKey = keyPair.getPrivate();
        PublicKey publicKey = keyPair.getPublic();
        return new MutablePair<>(StringUtils.bytesToHex(publicKey.getEncoded()),StringUtils.bytesToHex(privateKey.getEncoded()));
    }

    /**
     * 生成SM2密钥对
     * @return {@link java.security.KeyPair}
     * @author anwen
     * @date 2024/6/30 上午1:23
     */
    public static MutablePair<String,String> generateSM2Key() throws Exception {
        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("EC", "BC");
        keyPairGenerator.initialize(new ECGenParameterSpec("sm2p256v1"));
        KeyPair keyPair = keyPairGenerator.generateKeyPair();
        PrivateKey privateKey = keyPair.getPrivate();
        PublicKey publicKey = keyPair.getPublic();
        return new MutablePair<>(Hex.toHexString(publicKey.getEncoded()),Hex.toHexString(privateKey.getEncoded()));
    }

    /**
     * 生成SM4秘钥
     * @return {@link String}
     * @author anwen
     * @date 2024/7/1 下午3:50
     */
    public static String generateSM4Key() throws Exception {
        KeyGenerator kg = KeyGenerator.getInstance("SM4", BouncyCastleProvider.PROVIDER_NAME);
        kg.init(128); // SM4使用128位密钥
        SecretKey secretKey = kg.generateKey();
        return Hex.toHexString(secretKey.getEncoded());
    }

}
