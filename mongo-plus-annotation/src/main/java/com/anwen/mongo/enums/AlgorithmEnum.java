package com.anwen.mongo.enums;

/**
 * 加密类型
 *
 * @author anwen
 * @date 2024/6/30 下午1:41
 */
public enum AlgorithmEnum {

    MD5_32("MD5_32"),

    MD5_16("MD5_16"),

    BASE64("BASE64"),

    AES("AES"),

    RSA("RSA"),

    SM2("SM2"),

    SM3("SM3"),

    SM4("SM4"),

    PBEWithMD5AndDES("PBEWithMD5AndDES"),

    PBEWithMD5AndTripleDES("PBEWithMD5AndTripleDES"),

    PBEWithSHA1AndDESede("PBEWithSHA1AndDESede"),

    PBEWithSHA1AndRC2_40("PBEWithSHA1AndRC2_40"),

    ;

    private final String algorithm;

    public String getAlgorithm() {
        return algorithm;
    }

    AlgorithmEnum(String algorithm) {
        this.algorithm = algorithm;
    }
}
