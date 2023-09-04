package com.anwen.mongo.enums;

import java.util.Objects;

/**
 * @author JiaChaoYang
 **/
public enum AggregateOptionsEnum {

    ALLOW_DISK_USE("allowDiskUse"),

    BATCH_SIZE("batchSize"),

    COLLATION("collation"),

    MAX_TIME_MS("maxTimeMS"),

    MAX_AWAIT_TIME_MS("maxAwaitTimeMS"),

    BYPASS_DOCUMENT_VALIDATION("bypassDocumentValidation"),

    COMMENT("comment"),

    COMMENT_STR("comment_str"),

    HINT("hint"),

    HINT_STR("hint"),

    LET("let"),

    ;

    private final String options;

    public String getOptions() {
        return options;
    }

    AggregateOptionsEnum(String options) {
        this.options = options;
    }

    public static AggregateOptionsEnum getByOptions(String options){
        for(AggregateOptionsEnum aggregateOptionsEnum:values()) {
            if(Objects.equals(aggregateOptionsEnum.options, options)) {
                return aggregateOptionsEnum;
            }
        }
        return null;
    }

}
