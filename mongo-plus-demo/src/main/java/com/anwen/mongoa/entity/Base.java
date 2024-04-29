package com.anwen.mongoa.entity;

import com.anwen.mongo.annotation.collection.CollectionLogic;
import lombok.Data;

@Data
public class Base {

    //    @TableLogic(delval = "0", value = "1", close = true)
    @CollectionLogic
    private String logicDel;

}
