package com.uhu.entity;

import com.anwen.mongo.annotation.ID;
import lombok.Data;

@Data
public class User {
    //使用ID注解，标注此字段为MongoDB的_id，或者继承BaseModelID类
    @ID
    private String id;
    private String name;
    private Long age;
    private String email;
}
