package com.anwen.mongo.domain;

/**
 * @author JiaChaoYang
 * 查询异常
 * @since 2023-02-10 13:37
 **/
public class MongoQueryException extends MongoPlusException {

    private String message;

    @Override
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public MongoQueryException(String message){
        super(message);
        this.message = message;
    }

}
