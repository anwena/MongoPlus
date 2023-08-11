package com.anwen.mongo.model;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.enums.IdTypeEnum;
import lombok.Data;

/**
 * @author JiaChaoYang
 * 基础对象ID
 * @since 2023-02-13 11:52
 **/
@Data
public class BaseModelID {

    /**
     * mongoDB生成的id
     * @since 2023/2/13 11:52
    */
    @ID(type = IdTypeEnum.ASSIGN_ID)
    @CollectionField("_id")
    public String id;
}
