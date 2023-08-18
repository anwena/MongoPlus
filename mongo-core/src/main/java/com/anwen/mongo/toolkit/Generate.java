package com.anwen.mongo.toolkit;

import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.id.IdWorker;
import com.anwen.mongo.id.ObjectId;
import com.github.f4b6a3.ulid.UlidCreator;

import java.util.UUID;

/**
 * @author JiaChaoYang
 **/
public class Generate {


    public static String generateId(IdTypeEnum idTypeEnum){
        if (idTypeEnum.getKey() == IdTypeEnum.OBJECT_ID.getKey()){
            return ObjectId.next(false);
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_UUID.getKey()){
            return UUID.randomUUID().toString().replaceAll("-","");
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_ULID.getKey()){
            return UlidCreator.getMonotonicUlid().toLowerCase();
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_ID.getKey()){
            return String.valueOf(IdWorker.getId());
        }
        return null;
    }


}
