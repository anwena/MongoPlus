package com.anwen.mongo.domain;

/**
 * MongoPlus异常
 *
 * @author JiaChaoYang
 **/
public class MongoPlusException extends RuntimeException {

    String message;

    public MongoPlusException(String message) {
        super(message);
        this.message = message;
    }

    @Override
    public String getMessage() {
        return message;
    }
}
