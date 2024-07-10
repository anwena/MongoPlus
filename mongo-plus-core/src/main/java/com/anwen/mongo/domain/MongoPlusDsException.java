package com.anwen.mongo.domain;

/**
 * MongoPlus动态数据源异常
 *
 * @author anwen
 * @date 2024/7/9 上午11:23
 */
public class MongoPlusDsException extends MongoPlusException {

    public MongoPlusDsException() {
        super("No data source found");
    }

    public MongoPlusDsException(String message) {
        super(message);
    }
}
