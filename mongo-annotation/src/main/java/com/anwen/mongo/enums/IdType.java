package com.anwen.mongo.enums;

import com.anwen.mongo.generate.ObjectId;
import com.anwen.mongo.generate.Sequence;
import com.github.f4b6a3.ulid.Ulid;
import com.github.f4b6a3.ulid.UlidCreator;
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
     * 生成mongoDB自带的_id
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:32
     **/
    OBJECT_ID(0),

    /**
     * 生成UUID
     * @since 2023/2/13 16:09
    */
    ASSIGN_UUID(1),

    /**
     * 生成ULID，效率相比UUID要高
     * @author: JiaChaoYang
     * @date: 2023/6/7 21:45
     **/
    ASSIGN_ULID(2),

    /**
     * 生成雪花算法
     * @since 2023/2/13 16:09
    */
    ASSIGN_ID(3)

    ;

    private final int key;

    public static String generateId(IdType idType){
        if (idType.getKey() == OBJECT_ID.getKey()){
            return ObjectId.next(false);
        }
        if (idType.getKey() == ASSIGN_UUID.getKey()){
            return UUID.randomUUID().toString().replaceAll("-","");
        }
        if (idType.getKey() == ASSIGN_ULID.getKey()){
            return UlidCreator.getMonotonicUlid().toLowerCase();
        }
        if (idType.getKey() == ASSIGN_ID.getKey()){
            return String.valueOf(new Sequence(null).nextId());
        }
        return null;
    }
}
