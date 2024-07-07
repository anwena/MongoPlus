package com.anwen.mongo.toolkit;

import com.anwen.mongo.enums.DesensitizationTypeEnum;

/**
 * 脱敏工具类
 *
 * @author anwen
 * @date 2024/6/28 下午4:08
 */
public class DesensitizedUtil {

    public static final char SPACE = ' ';

    /**
     * 脱敏，使用默认的脱敏策略
     * <pre>
     * DesensitizedUtil.desensitized("100", DesensitizedUtil.DesensitizedType.USER_ID)) =  "0"
     * DesensitizedUtil.desensitized("段正淳", DesensitizedUtil.DesensitizedType.CHINESE_NAME)) = "段**"
     * DesensitizedUtil.desensitized("51343620000320711X", DesensitizedUtil.DesensitizedType.ID_CARD)) = "5***************1X"
     * DesensitizedUtil.desensitized("09157518479", DesensitizedUtil.DesensitizedType.FIXED_PHONE)) = "0915*****79"
     * DesensitizedUtil.desensitized("18049531999", DesensitizedUtil.DesensitizedType.MOBILE_PHONE)) = "180****1999"
     * DesensitizedUtil.desensitized("北京市海淀区马连洼街道289号", DesensitizedUtil.DesensitizedType.ADDRESS)) = "北京市海淀区马********"
     * DesensitizedUtil.desensitized("duandazhi-jack@gmail.com.cn", DesensitizedUtil.DesensitizedType.EMAIL)) = "d*************@gmail.com.cn"
     * DesensitizedUtil.desensitized("1234567890", DesensitizedUtil.DesensitizedType.PASSWORD)) = "**********"
     * DesensitizedUtil.desensitized("苏D40000", DesensitizedUtil.DesensitizedType.CAR_LICENSE)) = "苏D4***0"
     * DesensitizedUtil.desensitized("11011111222233333256", DesensitizedUtil.DesensitizedType.BANK_CARD)) = "1101 **** **** **** 3256"
     * DesensitizedUtil.desensitized("192.168.1.1", DesensitizedUtil.DesensitizedType.IPV4)) = "192.*.*.*"
     * </pre>
     *
     * @param str              字符串
     * @param desensitizedType 脱敏类型;可以脱敏：用户id、中文名、身份证号、座机号、手机号、地址、电子邮件、密码
     * @return 脱敏之后的字符串
     * @author dazer and neusoft and qiaomu
     * @since 5.6.2
     */
    public static String desensitized(CharSequence str,int startInclude,int endExclude, DesensitizationTypeEnum desensitizedType) {
        if (StringUtils.isBlank(str)) {
            return StringPool.EMPTY;
        }
        String newStr = String.valueOf(str);
        switch (desensitizedType) {
            case CUSTOM:
                newStr = StringUtils.hide(str,startInclude,endExclude);
                break;
            case USER_ID:
                newStr = String.valueOf(userId());
                break;
            case CHINESE_NAME:
                newStr = chineseName(String.valueOf(str));
                break;
            case ID_CARD:
                newStr = idCardNum(String.valueOf(str), 1, 2);
                break;
            case FIXED_PHONE:
                newStr = fixedPhone(String.valueOf(str));
                break;
            case MOBILE_PHONE:
                newStr = mobilePhone(String.valueOf(str));
                break;
            case ADDRESS:
                newStr = address(String.valueOf(str), 8);
                break;
            case EMAIL:
                newStr = email(String.valueOf(str));
                break;
            case PASSWORD:
                newStr = password(String.valueOf(str));
                break;
            case CAR_LICENSE:
                newStr = carLicense(String.valueOf(str));
                break;
            case BANK_CARD:
                newStr = bankCard(String.valueOf(str));
                break;
            case IPV4:
                newStr = ipv4(String.valueOf(str));
                break;
            case IPV6:
                newStr = ipv6(String.valueOf(str));
                break;
            case FIRST_MASK:
                newStr = firstMask(String.valueOf(str));
                break;
            case CLEAR_TO_EMPTY:
                newStr = clear();
                break;
            case CLEAR_TO_NULL:
                newStr = clearToNull();
                break;
            default:
        }
        return newStr;
    }

    /**
     * 清空为空字符串
     *
     * @return 清空后的值
     * @since 5.8.22
     */
    public static String clear() {
        return StringPool.EMPTY;
    }

    /**
     * 清空为{@code null}
     *
     * @return 清空后的值(null)
     * @since 5.8.22
     */
    public static String clearToNull() {
        return null;
    }

    /**
     * 【用户id】不对外提供userId
     *
     * @return 脱敏后的主键
     */
    public static Long userId() {
        return 0L;
    }

    /**
     * 定义了一个first_mask的规则，只显示第一个字符。<br>
     * 脱敏前：123456789；脱敏后：1********。
     *
     * @param str 字符串
     * @return 脱敏后的字符串
     */
    public static String firstMask(String str) {
        if (StringUtils.isBlank(str)) {
            return StringPool.EMPTY;
        }
        return StringUtils.hide(str, 1, str.length());
    }

    /**
     * 【中文姓名】只显示第一个汉字，其他隐藏为2个星号，比如：李**
     *
     * @param fullName 姓名
     * @return 脱敏后的姓名
     */
    public static String chineseName(String fullName) {
        return firstMask(fullName);
    }

    /**
     * 【身份证号】前1位 和后2位
     *
     * @param idCardNum 身份证
     * @param front     保留：前面的front位数；从1开始
     * @param end       保留：后面的end位数；从1开始
     * @return 脱敏后的身份证
     */
    public static String idCardNum(String idCardNum, int front, int end) {
        //身份证不能为空
        if (StringUtils.isBlank(idCardNum)) {
            return StringPool.EMPTY;
        }
        //需要截取的长度不能大于身份证号长度
        if ((front + end) > idCardNum.length()) {
            return StringPool.EMPTY;
        }
        //需要截取的不能小于0
        if (front < 0 || end < 0) {
            return StringPool.EMPTY;
        }
        return StringUtils.hide(idCardNum, front, idCardNum.length() - end);
    }

    /**
     * 【固定电话 前四位，后两位
     *
     * @param num 固定电话
     * @return 脱敏后的固定电话；
     */
    public static String fixedPhone(String num) {
        if (StringUtils.isBlank(num)) {
            return StringPool.EMPTY;
        }
        return StringUtils.hide(num, 4, num.length() - 2);
    }

    /**
     * 【手机号码】前三位，后4位，其他隐藏，比如135****2210
     *
     * @param num 移动电话；
     * @return 脱敏后的移动电话；
     */
    public static String mobilePhone(String num) {
        if (StringUtils.isBlank(num)) {
            return StringPool.EMPTY;
        }
        return StringUtils.hide(num, 3, num.length() - 4);
    }

    /**
     * 【地址】只显示到地区，不显示详细地址，比如：北京市海淀区****
     *
     * @param address       家庭住址
     * @param sensitiveSize 敏感信息长度
     * @return 脱敏后的家庭地址
     */
    public static String address(String address, int sensitiveSize) {
        if (StringUtils.isBlank(address)) {
            return StringPool.EMPTY;
        }
        int length = address.length();
        return StringUtils.hide(address, length - sensitiveSize, length);
    }

    /**
     * 【电子邮箱】邮箱前缀仅显示第一个字母，前缀其他隐藏，用星号代替，@及后面的地址显示，比如：d**@126.com
     *
     * @param email 邮箱
     * @return 脱敏后的邮箱
     */
    public static String email(String email) {
        if (StringUtils.isBlank(email)) {
            return StringPool.EMPTY;
        }
        int index = StringUtils.indexOf(email, "@");
        if (index <= 1) {
            return email;
        }
        return StringUtils.hide(email, 1, index);
    }

    /**
     * 【密码】密码的全部字符都用*代替，比如：******
     *
     * @param password 密码
     * @return 脱敏后的密码
     */
    public static String password(String password) {
        if (StringUtils.isBlank(password)) {
            return StringPool.EMPTY;
        }
        return StringUtils.repeat('*', password.length());
    }

    /**
     * 【中国车牌】车牌中间用*代替
     * eg1：null       -》 ""
     * eg1：""         -》 ""
     * eg3：苏D40000   -》 苏D4***0
     * eg4：陕A12345D  -》 陕A1****D
     * eg5：京A123     -》 京A123     如果是错误的车牌，不处理
     *
     * @param carLicense 完整的车牌号
     * @return 脱敏后的车牌
     */
    public static String carLicense(String carLicense) {
        if (StringUtils.isBlank(carLicense)) {
            return StringPool.EMPTY;
        }
        // 普通车牌
        if (carLicense.length() == 7) {
            carLicense = StringUtils.hide(carLicense, 3, 6);
        } else if (carLicense.length() == 8) {
            // 新能源车牌
            carLicense = StringUtils.hide(carLicense, 3, 7);
        }
        return carLicense;
    }

    /**
     * 【银行卡号脱敏】由于银行卡号长度不定，所以只展示前4位，后面的位数根据卡号决定展示1-4位
     * 例如：
     * <pre>{@code
     *      1. "1234 2222 3333 4444 6789 9"    ->   "1234 **** **** **** **** 9"
     *      2. "1234 2222 3333 4444 6789 91"   ->   "1234 **** **** **** **** 91"
     *      3. "1234 2222 3333 4444 678"       ->    "1234 **** **** **** 678"
     *      4. "1234 2222 3333 4444 6789"      ->    "1234 **** **** **** 6789"
     *  }</pre>
     *
     * @param bankCardNo 银行卡号
     * @return 脱敏之后的银行卡号
     */
    public static String bankCard(String bankCardNo) {
        if (StringUtils.isBlank(bankCardNo)) {
            return bankCardNo;
        }
        bankCardNo = StringUtils.cleanBlank(bankCardNo);
        if (bankCardNo.length() < 9) {
            return bankCardNo;
        }

        final int length = bankCardNo.length();
        final int endLength = length % 4 == 0 ? 4 : length % 4;
        final int midLength = length - 4 - endLength;

        final StringBuilder buf = new StringBuilder();
        buf.append(bankCardNo, 0, 4);
        for (int i = 0; i < midLength; ++i) {
            if (i % 4 == 0) {
                buf.append(SPACE);
            }
            buf.append('*');
        }
        buf.append(SPACE).append(bankCardNo, length - endLength, length);
        return buf.toString();
    }

    /**
     * IPv4脱敏，如：脱敏前：192.0.2.1；脱敏后：192.*.*.*。
     *
     * @param ipv4 IPv4地址
     * @return 脱敏后的地址
     */
    public static String ipv4(String ipv4) {
        return StringUtils.subBefore(ipv4, '.', false) + ".*.*.*";
    }

    /**
     * IPv4脱敏，如：脱敏前：2001:0db8:86a3:08d3:1319:8a2e:0370:7344；脱敏后：2001:*:*:*:*:*:*:*
     *
     * @param ipv6 IPv4地址
     * @return 脱敏后的地址
     */
    public static String ipv6(String ipv6) {
        return StringUtils.subBefore(ipv6, ':', false) + ":*:*:*:*:*:*:*";
    }
    
}
