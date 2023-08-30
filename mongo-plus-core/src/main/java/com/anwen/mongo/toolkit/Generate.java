package com.anwen.mongo.toolkit;

import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.incrementer.id.IdWorker;
import org.bson.types.ObjectId;

/**
 * @author JiaChaoYang
 **/
public class Generate {


    public static String generateId(IdTypeEnum idTypeEnum){
        if (idTypeEnum.getKey() == IdTypeEnum.OBJECT_ID.getKey()){
            return ObjectId.get().toHexString();
        }
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
