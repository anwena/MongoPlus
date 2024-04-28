package com.anwen.mongo.domain;

/**
 * 初始化MongoPlus异常
 *
 * @author JiaChaoYang
 **/
public class InitMongoPlusException extends MongoPlusException {

    public InitMongoPlusException(String message){
        super(message);
        this.message = message;
    }

}
