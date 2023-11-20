package com.uhu.controller;

import com.alibaba.fastjson.JSONObject;
import com.uhu.entity.User;
import com.uhu.mapper.UserMapper;
import com.uhu.service.UserService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * @Description:
 * @Name: UserController
 * @Author: Bomber
 * @CreateTime: 2023/11/16 10:09
 */
@RestController
public class UserController {

    @Resource
    private UserMapper userMapper;

    @Resource
    private UserService userService;

    @GetMapping("/user/{id}")
    public User getById(@PathVariable Integer id) {
        return userMapper.getById(id);
    }

    @GetMapping("/user2/{id}")
    public JSONObject getById2(@PathVariable Integer id) {
        return userService.getById(id);
    }
}
