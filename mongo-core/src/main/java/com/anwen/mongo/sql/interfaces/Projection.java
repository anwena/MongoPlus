package com.anwen.mongo.sql.interfaces;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 显示字段
 *
 * @author JiaChaoYang
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Projection {

    private String column;

    private Object value;
}
