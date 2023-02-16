package com.anwen.mongo.enums;

import com.anwen.mongo.generate.Sequence;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

/**
 * @author JiaChaoYang
 * id生成类型
 * @since 2023-02-13 15:59
 **/
@Getter
@AllArgsConstructor
public enum IdType {

    /**
     * 生成UUID
     * @since 2023/2/13 16:09
    */
    ASSIGN_UUID(0),

    /**
     * 生成雪花算法
     * @since 2023/2/13 16:09
    */
    ASSIGN_ID(1)

    ;

    private final int key;
    public static String generateId(IdType idType){
        if (idType.getKey() == ASSIGN_UUID.getKey()){
            return UUID.randomUUID().toString().replaceAll("-","");
        }
        if (idType.getKey() == ASSIGN_ID.getKey()){
            return String.valueOf(new Sequence(null).nextId());
        }
        return null;
    }
}
