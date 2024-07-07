package com.anwen.mongo.toolkit;

import com.anwen.mongo.annotation.comm.FieldEncrypt;
import com.anwen.mongo.encryptor.*;
import com.anwen.mongo.enums.AlgorithmEnum;
import com.anwen.mongo.logging.Log;
import com.anwen.mongo.logging.LogFactory;

import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.HashMap;
import java.util.Map;

/**
 * @author anwen
 * @date 2024/6/30 上午12:46
 */
public class EncryptorUtil {

    private static final Log log = LogFactory.getLog(EncryptorUtil.class);

    public final static Map<AlgorithmEnum, Encryptor> algorithmEnumEncryptorMap = new HashMap<>();

    public final static Map<Class<?>,Encryptor> encryptorCache = new HashMap<>();

    static {
        algorithmEnumEncryptorMap.put(AlgorithmEnum.MD5_16,new MD5Example(16));
        algorithmEnumEncryptorMap.put(AlgorithmEnum.MD5_32,new MD5Example());
        algorithmEnumEncryptorMap.put(AlgorithmEnum.AES,new AESExample());
        algorithmEnumEncryptorMap.put(AlgorithmEnum.BASE64,new Base64Example());
        algorithmEnumEncryptorMap.put(AlgorithmEnum.RSA,new RSAExample());
        algorithmEnumEncryptorMap.put(AlgorithmEnum.PBEWithMD5AndDES,new PBEExample(AlgorithmEnum.PBEWithMD5AndDES.getAlgorithm()));
        algorithmEnumEncryptorMap.put(AlgorithmEnum.PBEWithMD5AndTripleDES,new PBEExample(AlgorithmEnum.PBEWithMD5AndTripleDES.getAlgorithm()));
        algorithmEnumEncryptorMap.put(AlgorithmEnum.PBEWithSHA1AndDESede,new PBEExample(AlgorithmEnum.PBEWithSHA1AndDESede.getAlgorithm()));
        algorithmEnumEncryptorMap.put(AlgorithmEnum.PBEWithSHA1AndRC2_40,new PBEExample(AlgorithmEnum.PBEWithSHA1AndRC2_40.getAlgorithm()));
    }

    public static Object encrypt(FieldEncrypt fieldEncrypt,Object value){
        try {
            Encryptor encryptor = getEncryptor(fieldEncrypt);
            value = encryptor.encrypt(String.valueOf(value),fieldEncrypt.key(),fieldEncrypt.publicKey());
        } catch (Exception e) {
            log.error(fieldEncrypt.algorithm().name()+" encryption failed due to: {}",e.getMessage(),e);
        }
        return value;
    }

    public static Object decrypt(FieldEncrypt fieldEncrypt,Object value){
        try {
            Encryptor encryptor = getEncryptor(fieldEncrypt);
            value = encryptor.decrypt(String.valueOf(value),fieldEncrypt.key(),fieldEncrypt.publicKey());
        } catch (Exception e) {
            log.error(fieldEncrypt.algorithm().name()+" decryption failed due to: {}",e.getMessage(),e);
        }
        return value;
    }

    public static Encryptor getEncryptor(FieldEncrypt fieldEncrypt){
        Encryptor encryptor = algorithmEnumEncryptorMap.get(fieldEncrypt.algorithm());
        if (fieldEncrypt.algorithm() == AlgorithmEnum.SM2){
            encryptor = new SM2Example();
            algorithmEnumEncryptorMap.put(AlgorithmEnum.SM2,encryptor);
        } else if (fieldEncrypt.algorithm() == AlgorithmEnum.SM3){
            encryptor = new SM3Example();
            algorithmEnumEncryptorMap.put(AlgorithmEnum.SM3,encryptor);
        } else if (fieldEncrypt.algorithm() == AlgorithmEnum.SM4){
            encryptor = new SM4Example();
            algorithmEnumEncryptorMap.put(AlgorithmEnum.SM4,encryptor);
        }
        if (fieldEncrypt.encryptor() != Void.class){
            if (encryptorCache.get(fieldEncrypt.encryptor()) != null){
                encryptor = encryptorCache.get(fieldEncrypt.encryptor());
            }else {
                encryptor = (Encryptor) ClassTypeUtil.getInstanceByClass(fieldEncrypt.encryptor());
                encryptorCache.put(fieldEncrypt.encryptor(),encryptor);
            }
        }
        return encryptor;
    }

    /**
     * 从十六进制字符串恢复公钥
     * @param key key
     * @param algorithm 算法名称
     * @return {@link java.security.PublicKey}
     * @author anwen
     * @date 2024/6/30 上午12:37
     */
    public static PublicKey getPublicKeyFromString(String key,String algorithm) throws Exception {
        byte[] keyBytes = StringUtils.hexToBytes(key);
        X509EncodedKeySpec spec = new X509EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance(algorithm);
        return keyFactory.generatePublic(spec);
    }

    /**
     * 从十六进制字符串恢复公钥
     * @param key key
     * @param algorithm 算法名称
     * @return {@link java.security.PublicKey}
     * @author anwen
     * @date 2024/6/30 上午12:37
     */
    public static PublicKey getPublicKeyFromString(String key,String algorithm,String provider) throws Exception {
        byte[] keyBytes = StringUtils.hexToBytes(key);
        X509EncodedKeySpec spec = new X509EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance(algorithm,provider);
        return keyFactory.generatePublic(spec);
    }

    /**
     * 从十六进制字符串恢复私钥
     * @param key key
     * @param algorithm 算法名称
     * @return {@link java.security.PrivateKey}
     * @author anwen
     * @date 2024/6/30 上午12:37
     */
    public static PrivateKey getPrivateKeyFromString(String key,String algorithm) throws Exception {
        byte[] keyBytes = StringUtils.hexToBytes(key);
        PKCS8EncodedKeySpec spec = new PKCS8EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance(algorithm);
        return keyFactory.generatePrivate(spec);
    }

    /**
     * 从十六进制字符串恢复私钥
     * @param key key
     * @param algorithm 算法名称
     * @return {@link java.security.PrivateKey}
     * @author anwen
     * @date 2024/6/30 上午12:37
     */
    public static PrivateKey getPrivateKeyFromString(String key,String algorithm,String provider) throws Exception {
        byte[] keyBytes = StringUtils.hexToBytes(key);
        PKCS8EncodedKeySpec spec = new PKCS8EncodedKeySpec(keyBytes);
        KeyFactory keyFactory = KeyFactory.getInstance(algorithm,provider);
        return keyFactory.generatePrivate(spec);
    }

}
