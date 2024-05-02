package com.anwen.mongo.toolkit;

import java.util.ArrayList;
import java.util.List;

public class IkunRandomUtil {

    private static final List<String> ikunLogList = new ArrayList<>();

    private static final List<String> ikunThreadList = new ArrayList<>();

    static {
        ikunThreadList.add("报错了，你干嘛哎呦  ");
        ikunThreadList.add("使用不规范，坤坤咬你蛋  ");
        ikunThreadList.add("漏出鸡脚了吧小黑子，这也能异常？  ");
        ikunLogList.add("中分篮球背带裤，我是ikun你记住  ");
        ikunLogList.add("春风又绿江南岸，练习时常两年半  ");
        ikunLogList.add("五花马，千金裘 ，鸡你太美好风流  ");
        ikunLogList.add("烟熏妆，护手霜，看它打球有点慌  ");
        ikunLogList.add("背带裤，增高鞋，裤腰拴着蝴蝶结  ");
        ikunLogList.add("能唱歌，能跳舞，不知是公还是母  ");
        ikunLogList.add("尔来四万八千岁，唱跳Rap全都会  ");
        ikunLogList.add("天若有情天亦老，香翅捞饭卤鸡脚  ");
        ikunLogList.add("春江水暖鸭先知，哥哥下蛋你别吃  ");
        ikunLogList.add("世间若是有真爱，必有中分加白带  ");
        ikunLogList.add("藏头又藏尾 只因你太美  ");
        ikunLogList.add("提笔我再话西游，唱跳rap打篮球  ");
        ikunLogList.add("莫笑他人穿破衣，背带中分篮球鸡  ");
        ikunLogList.add("披荆成王，伴kun远航，我们ikun不惹事也不怕事  ");
        ikunLogList.add("破天下定风云，我们爱kun并肩行  ");
        ikunLogList.add("向阳花木易为春，听说你爱菜虚鲲  ");
        ikunLogList.add("农夫山泉有点甜，不爱鲲鲲有点儿悬  ");
    }

    public static String getRandomLog(){
        int random = (int) (Math.random() * ikunLogList.size());
        return ikunLogList.get(random);
    }

    public static String getRandomThreadLog(){
        int random = (int) (Math.random() * ikunThreadList.size());
        return ikunThreadList.get(random);
    }

}
