package com.anwen.mongo.toolkit.encryptor;

import java.util.Base64;

/**
 * base64加密
 *
 * @author anwen
 * @date 2024/6/29 下午1:23
 */
public class Base64Example {

    /**
     * 使用Base64编码
     * @param original 原文
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/29 下午1:24
     */
    public static String encodeBase64(String original) {
        return Base64.getEncoder().encodeToString(original.getBytes());
    }

    /**
     * 使用Base64解码
     * @param original 原文
     * @return {@link java.lang.String}
     * @author anwen
     * @date 2024/6/29 下午1:24
     */
    public static String decodeBase64(String original) {
        return new String(Base64.getDecoder().decode(original));
    }

    public static void main(String[] args) {
        String original = "何玉楹";
        String s = encodeBase64(original);
        System.out.println(s);
        System.out.println(decodeBase64(s));
    }

}
