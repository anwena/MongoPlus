package com.anwen.mongoa.entity;

import com.anwen.mongo.annotation.TableLogic;
import lombok.Data;

@Data
public class Base {

    //    @TableLogic(delval = "0", value = "1", close = true)
    @TableLogic
    private String logicDel;

}
