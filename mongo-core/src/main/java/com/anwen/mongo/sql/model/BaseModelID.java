package com.anwen.mongo.sql.model;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.enums.IdType;

import java.util.UUID;

/**
 * @author JiaChaoYang
 * 基础对象ID
 * @since 2023-02-13 11:52
 **/
public class BaseModelID {

    /**
     * mongoDB生成的id
     * @since 2023/2/13 11:52
    */
    @ID(type = IdType.ASSIGN_ID)
    private String _id = UUID.randomUUID().toString().replaceAll("-","");

    public String get_id() {
        return _id;
    }

    public void set_id(String _id) {
        this._id = _id;
    }
}
