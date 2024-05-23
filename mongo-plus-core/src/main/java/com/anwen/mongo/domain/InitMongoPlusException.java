package com.anwen.mongo.domain;

/**
 * 初始化MongoPlus异常
 *
 * @author JiaChaoYang
 **/
public class InitMongoPlusException extends MongoPlusException {

    private String message;

    @Override
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public InitMongoPlusException(String message){
        super();
        this.message = message;
    }

    public InitMongoPlusException(){
        super();
    }

}
