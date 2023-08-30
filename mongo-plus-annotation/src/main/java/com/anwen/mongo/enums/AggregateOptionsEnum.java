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

    READ_CONCERN("readConcern"),

    WRITE_CONCERN("writeConcern"),

    ;

    private final String options;

    public String getOptions() {
        return options;
    }

    AggregateOptionsEnum(String options) {
        this.options = options;
    }

/*    public static AggregateOptionsEnum valueOf(String options){
        for(AggregateOptionsEnum aggregateOptionsEnum:values()) {
            if(Objects.equals(aggregateOptionsEnum.options, options)) {
                return aggregateOptionsEnum;
            }
        }
        return null;
    }*/

}
