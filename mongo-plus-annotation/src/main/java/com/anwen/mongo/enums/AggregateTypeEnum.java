package com.anwen.mongo.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author JiaChaoYang
 **/
@Getter
@AllArgsConstructor
public enum AggregateTypeEnum {

    PROJECT("project"),

    MATCH("match"),

    LIMIT("limit"),

    SKIP("skip"),

    SORT("sort"),

    GROUP("group"),

    LOOKUP("lookup"),

    ADD_FIELDS("addFields"),

    UNWIND("unwind"),

    REPLACE_ROOT("replaceRoot"),

    SAMPLE("sample"),

    UNION_WITH("unionWith"),

    OUT("out")

    ;

    private final String type;

}
