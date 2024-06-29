package com.anwen.mongo.toolkit.encryptor;

import javax.crypto.*;
import javax.crypto.spec.SecretKeySpec;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

/**
 * AES对称加密
 *
 * @author anwen
 * @date 2024/6/29 下午1:25
 */
public class AESExample {

    private static final String ALGORITHM = "AES";

    /**
     * AES加密
     * @param data 密文
     * @param password 密码
     * @return {@link byte[]}
     * @author anwen
     * @date 2024/6/29 下午1:26
     */
    public static String encrypt(String data, String password) throws Exception {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, getKeyFromPassword(password));
        StringBuilder hexString = new StringBuilder();
        for (byte hashByte : cipher.doFinal(data.getBytes())) {
            String hex = Integer.toHexString(0xff & hashByte);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }

    /**
     * AES解密
     * @param encryptedData 密文
     * @param password 密码
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/29 下午1:27
     */
    public static String decrypt(String encryptedData, String password) throws Exception {
        Cipher cipher = Cipher.getInstance(ALGORITHM);
        cipher.init(Cipher.DECRYPT_MODE, getKeyFromPassword(password));
        return new String(cipher.doFinal(hexToBytes(encryptedData)));
    }

    /**
     * 生成AES密钥
     * @return {@link javax.crypto.SecretKey}
     * @author anwen
     * @date 2024/6/29 下午1:27
     */
    public static SecretKey getKeyFromPassword(String password) throws NoSuchAlgorithmException {
        byte[] key = password.getBytes();
        MessageDigest sha = MessageDigest.getInstance("SHA-256");
        key = sha.digest(key);
        // 使用前16字节生成AES密钥
        key = Arrays.copyOf(key, 16);
        return new SecretKeySpec(key, ALGORITHM);
    }

    private static byte[] hexToBytes(String hex) {
        int len = hex.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4)
                    + Character.digit(hex.charAt(i+1), 16));
        }
        return data;
    }

    public static void main(String[] args) throws Exception {
        String name = "何玉楹";
        String password = "hyy";
        String encrypt = encrypt(name, password);
        System.out.println("密文："+encrypt);
        System.out.println(decrypt(encrypt,"123"));
    }

}
