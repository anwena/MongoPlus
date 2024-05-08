package com.anwen.mongo.cache.global;

import com.anwen.mongo.replacer.Replacer;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * executor 替换器
 *
 * @author loser
 * @date 2024/4/30
 */
public class ExecutorReplacerCache {

    public static List<Replacer> replacers = new ArrayList<>();

    public static void sorted() {
        replacers = replacers.stream().sorted(Comparator.comparing(Replacer::order)).collect(Collectors.toList());
    }

}
