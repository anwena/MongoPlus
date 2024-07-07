package com.anwen.mongo.enums;

/**
 * 条件枚举
 * @author JiaChaoYang
 * @date 2023/7/30 0:32
*/
public enum QueryOperatorEnum {

    MOD("mod"),

    LT("lt"),

    ELEM_MATCH("elemMatch"),

    TYPE("type"),

    NOR("nor"),

    NIN("nin"),

    NOT("not"),

    AND("and"),

    GTE("gte"),

    EXPR("expr"),

    LTE("lte"),

    ALL("all"),

    OR("or"),

    IN("in"),

    LIKE("like"),

    EQ("eq"),

    GT("gt"),

    REGEX("regex"),

    NE("ne"),

    TEXT("text"),

    EXISTS("exists"),

    SET("set"),

    INC("inc"),

    PUSH("push"),

    WHERE("where"),

    SIZE("size"),

    BITS_ALL_CLEAR("bitsAllClear"),

    BITS_ALL_SET("bitsAllSet"),

    BITS_ANY_CLEAR("bitsAnyClear"),

    BITS_ANY_SET("bitsAnySet"),

    ;

    private final String value;

    public String getValue() {
        return value;
    }

    QueryOperatorEnum(String value) {
        this.value = value;
    }

    public static boolean isQueryOperator(String value){
        QueryOperatorEnum queryOperator = getQueryOperator(value);
        return queryOperator != SET && queryOperator != PUSH && queryOperator != INC;
    }

    public static QueryOperatorEnum getQueryOperator(String value){
        for (QueryOperatorEnum queryOperatorEnum : QueryOperatorEnum.values()) {
            if (queryOperatorEnum.getValue().equals(value)){
                return queryOperatorEnum;
            }
        }
        return null;
    }

}
