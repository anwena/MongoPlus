package com.anwen.mongo.enums;

/**
 * 命令方法枚举
 *
 * @author loser
 * @date 2024/5/8
 */
public enum CommandEnum {

    FIND("find"),

    INSERT("insert"),

    DELETE("delete"),

    UPDATE("update"),

    AGGREGATE("aggregate"),

    COUNT("count"),

    ;

    private final String command;

    CommandEnum(String command) {
        this.command = command;
    }

    public String getCommand() {
        return command;
    }
}
