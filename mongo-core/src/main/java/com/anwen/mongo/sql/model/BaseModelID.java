package com.anwen.mongo.sql.model;

import com.anwen.mongo.annotation.ID;
import org.bson.codecs.pojo.annotations.BsonCreator;
import com.anwen.mongo.annotation.table.TableField;
import com.anwen.mongo.enums.IdType;
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
    @ID(type = IdType.ASSIGN_ID)
    @TableField("_id")
    public String id;
}
