package com.anwen.mongo.sql.conditions.interfaces.aggregate.project;

import com.anwen.mongo.sql.support.SFunction;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Project {

    private String column;

    private Object value;

    public static <T> Project p(SFunction<T,Object> column,Boolean value){
        return null;
    }

}
