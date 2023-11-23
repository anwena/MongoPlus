package com.anwen.mongo.domain;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 危险操作异常
 * @date 2023-11-23 11:57
 **/
public class BlockAttackInnerException extends Error {

    public BlockAttackInnerException(String message){
        super(message);
    }

}
