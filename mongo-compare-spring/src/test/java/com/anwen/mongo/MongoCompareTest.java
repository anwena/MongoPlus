package com.anwen.mongo;

import com.alibaba.fastjson.JSON;
import com.anwen.mongo.entity.Compare;
import com.anwen.mongo.mapper.BaseMapper;
import com.anwen.mongo.toolkit.BsonUtil;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;

import javax.annotation.Resource;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * MongoPlus-MongoTemplate查询效率对比测试类
 * @author anwen
 */
@SpringBootTest
@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MongoCompareTest {

    @Resource
    private BaseMapper baseMapper;

    @Resource
    private MongoTemplate mongoTemplate;

    @Test
    @Order(0)
    public void init(){
        mongoPlusInit();
        mongoDataInit();
    }

    @Test
    @Order(1)
    public void compare(){
        Long mongoPlus = mongoPlusFind();
        Long mongoTemplate = mongoDataFind();
        log.info("MongoPlus耗时：{}",mongoPlus);
        log.info("MongoTemplate耗时：{}",mongoTemplate);
        log.info("MongoPlus较MongoTemplate查询高：{}%",new DecimalFormat("#.00").format(((mongoTemplate-mongoPlus)/(double)mongoTemplate)*100));
    }

    @Test
    @Order(2)
    public void compareAvg(){
        List<Long> mongoPlusList = new ArrayList<>();
        List<Long> mongoTemplateList = new ArrayList<>();
        for (int i=0;i<100;i++){
            mongoPlusList.add(mongoPlusFind());
            mongoTemplateList.add(mongoDataFind());
        }
        AtomicReference<Double> mongoPlus = new AtomicReference<>();
        AtomicReference<Double> mongoTemplate = new AtomicReference<>();
        mongoPlusList.stream().mapToLong(Long::longValue).average().ifPresent(mongoPlus::set);
        mongoTemplateList.stream().mapToLong(Long::longValue).average().ifPresent(mongoTemplate::set);
        log.info("MongoPlus平均耗时：{}",mongoPlus);
        log.info("MongoTemplate平均耗时：{}",mongoTemplate);
        log.info("MongoPlus较MongoTemplate平均值查询高：{}%",new DecimalFormat("#.00").format(((mongoTemplate.get()- mongoPlus.get())/(double)mongoTemplate.get())*100));
    }

    public Long mongoPlusFind() {
        log.info("#############################  MongoPlus start  ############################################");
        log.info("mongoPlus热身开始");
        baseMapper.list(Compare.class);
        log.info("mongoPlus热身结束");
        long action = System.currentTimeMillis();
        log.info("mongoPlus开始时间：{}",LocalDateTime.now());
        log.info("mongoPlus开始时间戳：{}",action);
        baseMapper.list(Compare.class);
        log.info("mongoPlus结束时间：{}",LocalDateTime.now());
        log.info("mongoPlus结束时间戳：{}",System.currentTimeMillis());
        long end = System.currentTimeMillis() - action;
        log.info("mongoPlus耗时：{}",end);
        log.info("#############################  MongoPlus end  ##############################################");
        return end;
    }

    public Long mongoDataFind() {
        log.info("#############################  MongoTemplate start  ############################################");
        log.info("mongoTemplate热身开始");
        mongoTemplate.findAll(Compare.class);
        log.info("mongoTemplate热身结束");
        long action = System.currentTimeMillis();
        log.info("mongoTemplate开始时间：{}",LocalDateTime.now());
        log.info("mongoTemplate开始时间戳：{}",action);
        mongoTemplate.findAll(Compare.class);
        log.info("mongoTemplate结束时间：{}",LocalDateTime.now());
        log.info("mongoTemplate结束时间戳：{}",System.currentTimeMillis());
        long end = System.currentTimeMillis() - action;
        log.info("mongoTemplate耗时：{}",end);
        log.info("#############################  MongoTemplate end  ##############################################");
        return end;
    }

    public void mongoPlusInit() {
        baseMapper.remove(BsonUtil.EMPTY_DOCUMENT, Compare.class);
        baseMapper.saveBatch(getCompareData());
    }

    public void mongoDataInit() {
        mongoTemplate.remove(new Query(),"compare");
        mongoTemplate.insertAll(getCompareData());
    }

    @SneakyThrows
    private List<Compare> getCompareData(){
        byte[] bytes = Files.readAllBytes(Paths.get(System.getProperty("user.dir") + "/json/content.json"));
        Compare compare = JSON.parseObject(bytes, Compare.class);
        return Stream.generate(() -> compare)
                .limit(10000)
                .collect(Collectors.toList());
    }

}
