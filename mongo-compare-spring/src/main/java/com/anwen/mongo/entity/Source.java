/*
 * Copyright (c) JiaChaoYang 2024-7-13 MongoPlus版权所有
 * 适度编码益脑，沉迷编码伤身，合理安排时间，享受快乐生活。
 * email: j15030047216@163.com
 * phone: 15030047216
 * weChat: JiaChaoYang_
 */

package com.anwen.mongo.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * @author anwen
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Source {

    private String msg;

    private Integer code;

    private LocalDateTime responseTime;

    private String channel;

    private Map<String,Object> responseJSON;

    private String source;

    private String reqId;

    private String vendorCode;

    private Map<String,Object> requestJSON;

    private LocalDateTime requestTime;

    private String application;

    private String action;

    private String region;

    private String status;

}
