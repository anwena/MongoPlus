package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author JiaChaoYang
 * id生成类型
 * @since 2023-02-13 15:59
 **/
@Getter
@AllArgsConstructor
public enum IdTypeEnum {

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
    ASSIGN_ID(3),

    /**
     * 生成自增id
     * <p style='color:red'>注：自增id会创建一个counters集合，用来存储当前id，请适当分配权限，新增也会慢那么一点点，着重考虑</p>
     * @author JiaChaoYang
     * @date 2023/8/9 22:39
    */
    AUTO(4)

    ;

    private final int key;

//    public static String generateId(IdTypeEnum idTypeEnum){
//        if (idTypeEnum.getKey() == OBJECT_ID.getKey()){
//            return ObjectId.next(false);
//        }
//        if (idTypeEnum.getKey() == ASSIGN_UUID.getKey()){
//            return UUID.randomUUID().toString().replaceAll("-","");
//        }
//        if (idTypeEnum.getKey() == ASSIGN_ULID.getKey()){
//            return UlidCreator.getMonotonicUlid().toLowerCase();
//        }
//        if (idTypeEnum.getKey() == ASSIGN_ID.getKey()){
//            return String.valueOf(new Sequence(null).nextId());
//        }
//        return null;
//    }
}
