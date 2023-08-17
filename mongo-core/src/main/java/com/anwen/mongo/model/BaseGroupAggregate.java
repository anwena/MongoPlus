package com.anwen.mongo.model;

import com.anwen.mongo.conditions.accumulator.Accumulator;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * 分组操作
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BaseGroupAggregate extends BasePipeline {

    /**
     * group条件
     * @author JiaChaoYang
     * @date 2023/8/17 22:48
    */
    private List<Accumulator> accumulatorList;

    public BaseGroupAggregate(Accumulator accumulator) {
        this.accumulatorList = new ArrayList<>(Collections.singleton(accumulator));
    }

    public BaseGroupAggregate(Accumulator[] accumulators) {
        this.accumulatorList = new ArrayList<>(Arrays.asList(accumulators));
    }
}