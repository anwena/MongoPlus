package com.anwen.mongo.domain;

/**
 * 写入异常
 *
 * @author JiaChaoYang
 **/
public class MongoPlusWriteException extends MongoPlusException {

    public MongoPlusWriteException(String message){
        super(message);
    }

}
