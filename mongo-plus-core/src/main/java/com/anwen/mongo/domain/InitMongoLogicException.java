package com.anwen.mongo.domain;

/**
 * 初始化MongoPlus逻辑删除异常
 *
 * @author loser
 * @date 2024/4/30
 */
public class InitMongoLogicException extends MongoPlusException {

    public InitMongoLogicException(String message) {
        super(message);
        this.message = message;
    }

}
