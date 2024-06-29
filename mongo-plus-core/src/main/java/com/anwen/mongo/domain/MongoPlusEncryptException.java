package com.anwen.mongo.domain;

/**
 * 加密异常类
 *
 * @author anwen
 * @date 2024/6/29 下午1:10
 */
public class MongoPlusEncryptException extends MongoPlusException {

    public MongoPlusEncryptException(String message, Throwable cause) {
        super(message, cause);
    }
}
