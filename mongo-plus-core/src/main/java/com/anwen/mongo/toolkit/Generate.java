package com.anwen.mongo.toolkit;

import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.incrementer.id.IdWorker;

import java.io.Serializable;

/**
 * @author JiaChaoYang
 **/
public class Generate {


    public static Serializable generateId(IdTypeEnum idTypeEnum){
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_UUID.getKey()){
            return IdWorker.get32UUID();
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_ULID.getKey()){
            return IdWorker.get26ULID();
        }
        if (idTypeEnum.getKey() == IdTypeEnum.ASSIGN_ID.getKey()){
            return IdWorker.getIdStr();
        }
        return null;
    }


}
