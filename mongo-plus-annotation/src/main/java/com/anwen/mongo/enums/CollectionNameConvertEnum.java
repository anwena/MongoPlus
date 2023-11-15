package com.anwen.mongo.enums;

import java.util.Objects;

/**
 * Collection名称映射枚举
 */
public enum CollectionNameConvertEnum {

    /**
     * 类名全小写
     */
    ALL_CHAR_LOWERCASE("all_char_lowercase"),

    /**
     * 类名首字母小写
     */
    FIRST_CHAR_LOWERCASE("first_char_lowercase"),

    /**
     * 类名
     */
    CLASS_NAME("class_name"),

    /**
     * 类名按驼峰转下划线
     */
    CAMEL_TO_UNDERLINE("camel_to_underline"),
    ;

    private String type;

    CollectionNameConvertEnum(String type) {
        this.type = type;
    }

    public static CollectionNameConvertEnum getConvert(String type){
        for (CollectionNameConvertEnum v:values()){
            if(Objects.equals(v.type, type.toLowerCase())) {
                return v;
            }
        }
        return ALL_CHAR_LOWERCASE;
    }

}
