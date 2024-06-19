package com.anwen.mongo.enums;

/**
 * 执行器方法枚举
 *
 * @author JiaChaoYang
 **/
public enum ExecuteMethodEnum {

    SAVE("executeSave"),

    REMOVE("executeRemove"),

    UPDATE("executeUpdate"),

    QUERY("executeQuery"),

    AGGREGATE_OLD("executeAggregateOld"),

    AGGREGATE("executeAggregate"),

    COUNT("executeCount"),

    BULK_WRITE("executeBulkWrite")

    ;

    private final String method;

    ExecuteMethodEnum(String method) {
        this.method = method;
    }

    public String getMethod() {
        return method;
    }
}
