package com.anwen.mongo.enums;

import java.util.Arrays;

/**
 * 执行器方法枚举
 *
 * @author JiaChaoYang
 **/
public enum ExecuteMethodEnum {

    /**
     * 新增方法
     * @date 2024/6/27 下午3:01
     */
    SAVE("executeSave"),

    /**
     * 删除方法
     * @date 2024/6/27 下午3:01
     */
    REMOVE("executeRemove"),

    /**
     * 修改方法
     * @date 2024/6/27 下午3:01
     */
    UPDATE_OLD("executeUpdateOld"),

    /**
     * 修改方法
     * @date 2024/6/27 下午3:01
     */
    UPDATE("executeUpdate"),

    /**
     * 查询方法
     * @date 2024/6/27 下午3:01
     */
    QUERY("executeQuery"),

    /**
     * 旧的管道执行方法
     * @author anwen
     * @date 2024/6/27 下午3:02
     */
    AGGREGATE_OLD("executeAggregateOld"),

    /**
     * 管道执行方法
     * @date 2024/6/27 下午3:02
     */
    AGGREGATE("executeAggregate"),

    /**
     * 统计方法
     * @date 2024/6/27 下午3:02
     */
    COUNT("executeCount"),

    /**
     * 批量执行方法
     * @date 2024/6/27 下午3:02
     */
    BULK_WRITE("executeBulkWrite")

    ;

    private final String method;

    ExecuteMethodEnum(String method) {
        this.method = method;
    }

    public String getMethod() {
        return method;
    }

    public static ExecuteMethodEnum getMethod(String method){
        return Arrays.stream(ExecuteMethodEnum.values())
                .filter(executeMethodEnumMethod -> executeMethodEnumMethod.getMethod().equals(method))
                .findFirst()
                .orElse(null);
    }

}
