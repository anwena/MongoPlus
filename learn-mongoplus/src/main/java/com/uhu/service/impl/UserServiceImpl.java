package com.uhu.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.anwen.mongo.service.impl.ServiceImpl;
import com.uhu.entity.User;
import com.uhu.service.UserService;
import org.springframework.stereotype.Service;

/**
 * @Description:
 * @Name: UserServiceImpl
 * @Author: Bomber
 * @CreateTime: 2023/11/20 9:56
 */
@Service
public class UserServiceImpl extends ServiceImpl<JSONObject> implements UserService {
}
