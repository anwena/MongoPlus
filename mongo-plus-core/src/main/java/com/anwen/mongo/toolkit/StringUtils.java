package com.anwen.mongo.toolkit;

import com.anwen.mongo.constant.SqlOperationConstant;
import com.mongodb.MongoException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.joining;

public final class StringUtils {

    static Logger logger = LoggerFactory.getLogger(StringUtils.class);

    /**
     * 字符串 is
     */
    public static final String IS = "is";
    /**
     * 下划线字符
     */
    public static final char UNDERLINE = '_';
    /**
     * MP 内定义的 SQL 占位符表达式，匹配诸如 {0},{1},{2} ... 的形式
     */
    public final static Pattern MP_SQL_PLACE_HOLDER = Pattern.compile("[{](?<idx>\\d+)}");
    /**
     * 验证字符串是否是数据库字段
     */
    private static final Pattern P_IS_COLUMN = Pattern.compile("^\\w\\S*[\\w\\d]*$");

    /**
     * 是否为大写命名
     */
    private static final Pattern CAPITAL_MODE = Pattern.compile("^[0-9A-Z/_]+$");

    /**
     * 字符串去除空白内容
     *
     * <ul> <li>\n 回车</li> <li>\t 水平制表符</li> <li>\s 空格</li> <li>\r 换行</li> </ul>
     */
    private static final Pattern REPLACE_BLANK = Pattern.compile("\\s*|\t|\r|\n");

    /**
     * 判断字符串中是否全是空白字符
     *
     * @param cs 需要判断的字符串
     * @return 如果字符串序列是 null 或者全是空白，返回 true
     */
    public static boolean isBlank(CharSequence cs) {
        if (cs != null) {
            int length = cs.length();
            for (int i = 0; i < length; i++) {
                if (!Character.isWhitespace(cs.charAt(i))) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * 对象转为字符串去除左右空格
     *
     * @param o 带转换对象
     * @return
     */
    public static String toStringTrim(Object o) {
        return String.valueOf(o).trim();
    }

    /**
     * @see #isBlank(CharSequence)
     */
    public static boolean isNotBlank(CharSequence cs) {
        return !isBlank(cs);
    }

    public static boolean isEmpty(CharSequence cs) {
        return cs == null || cs.length() == 0;
    }

    public static boolean isNotEmpty(CharSequence cs) {
        return !isEmpty(cs);
    }

    /**
     * @param str 字符串
     * @return 是否不空
     */
    public static boolean isNotBlank(String str) {
        return !isBlank(str);
    }

    public static String isNotBlankAndConvert(Object value){
        try {
            String str = String.valueOf(value);
            if (isBlank(str)){
                throw new MongoException("value is empty");
            }
            return str;
        } catch (Exception e) {
            logger.warn("Conversion to String failed, reason for failure: {}",e.getMessage());
            throw new MongoException("Conversion to String failed");
        }
    }

    /**
     * @param str 字符串
     * @return 是否为空
     */
    public static boolean isBlank(String str) {
        if (str == null) {
            return true;
        }
        if (str.equals("null")){
            return true;
        }
        int len = str.length();
        if (len == 0) {
            return true;
        }
        for (int i = 0; i < len; i++) {
            switch (str.charAt(i)) {
                case ' ':
                case '\t':
                case '\n':
                case '\r':
                    break;
                default:
                    return false;
            }
        }
        return true;
    }

    /**
     * 首字母小写
     * @param str 字符串
     * @return String
     */
    public static String firstCharToLowerCase(String str) {
        return changeFirstCharCase(str, false);
    }

    /**
     * 首字母大写
     * @param str 字符串
     * @return String
     */
    public static String firstCharToUpperCase(String str) {
        return changeFirstCharCase(str, true);
    }

    /**
     * 更改字符串第一个字符的大小写
     * @param str 字符串
     * @param capitalize true大写 false小写
     * @return 更改后的字符串
     */
    private static String changeFirstCharCase(String str, boolean capitalize) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        char baseChar = str.charAt(0);
        char updatedChar;
        if (capitalize) {
            updatedChar = Character.toUpperCase(baseChar);
        } else {
            updatedChar = Character.toLowerCase(baseChar);
        }

        if (baseChar == updatedChar) {
            return str;
        } else {
            char[] chars = str.toCharArray();
            chars[0] = updatedChar;
            return new String(chars);
        }
    }


    /**
     * 查找 src 里包含几个 target
     * @param src 源字符串
     * @param from 开始计数下标（包含）
     * @param to 结束计数下标（不包含）
     * @param targets 目标字符
     * @return 个数
     */
    public static int containCount(String src, int from, int to, char[] targets) {
        int count = 0;
        if (src != null) {
            from = Math.max(from, 0);
            to = Math.min(to, src.length());
            for (int i = from; i < to; i ++) {
                char c = src.charAt(i);
                boolean contained = false;
                for (char target : targets) {
                    if (c == target) {
                        contained = true;
                        break;
                    }
                }
                if (contained) {
                    count++;
                }
            }
        }
        return count;
    }

    /**
     * 驼峰风格风格转连字符风格
     * @param src 驼峰字符串
     * @param hyphenation 连字符
     * @return 连字符风格字符串
     */
    public static String toHyphenation(String src, String hyphenation) {
        StringBuilder sb = new StringBuilder(src);
        int cnt = 0;	// 插入连字符的个数
        for(int i = 1; i < src.length(); i++){
            if(Character.isUpperCase(src.charAt(i))){
                sb.insert(i + cnt, hyphenation);
                cnt += hyphenation.length();
            }
        }
        return sb.toString().toLowerCase();
    }

    /**
     * 驼峰风格风格转下划线风格
     * @param src 驼峰字符串
     * @return 下划风格字符串
     */
    public static String toUnderline(String src) {
        return toHyphenation(src, "_");
    }

    /**
     * 快速判断 SQL 片段中是否包含某个列
     * @param sql SQL 片段
     * @param column 列名
     * @return sql 中是否包含 column
     */
    public static boolean sqlContains(String sql, String column) {
        int cLen = column.length();
        int idx = sql.indexOf(column);
        while (idx >= 0) {
            if (idx > 0) {
                if (isSqlColumnChar(sql.charAt(idx - 1))) {
                    idx = sql.indexOf(column, idx + cLen);
                    continue;
                }
            }
            int endIdx = idx + cLen;
            if (endIdx < sql.length()) {
                if (isSqlColumnChar(sql.charAt(endIdx))) {
                    idx = sql.indexOf(column, idx + cLen);
                    continue;
                }
            }
            return true;
        }
        return false;
    }

    public static boolean isSqlColumnChar(char c) {
        return 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' || '0' <= c && c <= '9' || c == '_';
    }

    public static int countOf(String str, char target) {
        if (str == null) {
            return 0;
        }
        int count = 0;
        int index = str.indexOf(target);
        while (index >= 0) {
            if (index >= str.length()) {
                break;
            }
            index = str.indexOf(target, index + 1);
            count++;
        }
        return count;
    }

    /**
     * 判断字符串是不是驼峰命名
     *
     * <li> 包含 '_' 不算 </li>
     * <li> 首字母大写的不算 </li>
     *
     * @param str 字符串
     * @return 结果
     */
    public static boolean isCamel(String str) {
        return Character.isLowerCase(str.charAt(0)) && !str.contains(StringPool.UNDERSCORE);
    }

    /**
     * 判断字符串是否符合数据库字段的命名
     *
     * @param str 字符串
     * @return 判断结果
     */
    public static boolean isNotColumnName(String str) {
        return !P_IS_COLUMN.matcher(str).matches();
    }

    /**
     * 获取真正的字段名
     *
     * @param column 字段名
     * @return 字段名
     */
    public static String getTargetColumn(String column) {
        if (isNotColumnName(column)) {
            return column.substring(1, column.length() - 1);
        }
        return column;
    }

    /**
     * 字符串驼峰转下划线格式
     *
     * @param param 需要转换的字符串
     * @return 转换好的字符串
     */
    public static String camelToUnderline(String param) {
        if (isBlank(param)) {
            return StringPool.EMPTY;
        }
        int len = param.length();
        StringBuilder sb = new StringBuilder(len);
        for (int i = 0; i < len; i++) {
            char c = param.charAt(i);
            if (Character.isUpperCase(c) && i > 0) {
                sb.append(UNDERLINE);
            }
            sb.append(Character.toLowerCase(c));
        }
        return sb.toString();
    }

    /**
     * 字符串下划线转驼峰格式
     *
     * @param param 需要转换的字符串
     * @return 转换好的字符串
     */
    public static String underlineToCamel(String param) {
        if (isBlank(param)) {
            return StringPool.EMPTY;
        }
        String temp = param.toLowerCase();
        int len = temp.length();
        StringBuilder sb = new StringBuilder(len);
        for (int i = 0; i < len; i++) {
            char c = temp.charAt(i);
            if (c == UNDERLINE) {
                if (++i < len) {
                    sb.append(Character.toUpperCase(temp.charAt(i)));
                }
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * 下划线转驼峰
     * @author JiaChaoYang
     * @date 2023/10/20 20:45
    */
    public static String convertToCamelCase(String str) {
        if (Objects.equals(str, SqlOperationConstant._ID)){
            return str;
        }
        return Arrays.stream(str.split("_"))
                .reduce((s1, s2) -> s1 + s2.substring(0, 1).toUpperCase() + s2.substring(1))
                .orElse("");
    }

    /**
     * 正则表达式匹配
     *
     * @param regex 正则表达式字符串
     * @param input 要匹配的字符串
     * @return 如果 input 符合 regex 正则表达式格式, 返回true, 否则返回 false;
     */
    public static boolean matches(String regex, String input) {
        if (null == regex || null == input) {
            return false;
        }
        return Pattern.matches(regex, input);
    }

    /**
     * 替换 SQL 语句中的占位符，例如输入 SELECT * FROM test WHERE id = {0} AND name = {1} 会被替换为
     * SELECT * FROM test WHERE id = 1 AND name = 'MP'
     * <p>
     * 当数组中参数不足时，该方法会抛出错误：数组下标越界{@link ArrayIndexOutOfBoundsException}
     * </p>
     *
     * @param content 填充内容
     * @param args    填充参数
     */
    public static String sqlArgsFill(String content, Object... args) {
        if (StringUtils.isNotBlank(content) && ArrayUtils.isNotEmpty(args)) {
            // 索引不能使用，因为 SQL 中的占位符数字与索引不相同
            BiIntFunction<Matcher, CharSequence> handler = (m, i) -> sqlParam(args[Integer.parseInt(m.group("idx"))]);
            return replace(content, MP_SQL_PLACE_HOLDER, handler).toString();
        }
        return content;
    }

    /**
     * 根据指定的表达式替换字符串中指定格式的部分
     * <p>
     * BiIntFunction 中的 第二个 参数将传递 参数在字符串中的索引
     * </p>
     *
     * @param src      源字符串
     * @param ptn      需要替换部分的正则表达式
     * @param replacer 替换处理器
     * @return 返回字符串构建起
     */
    public static StringBuilder replace(CharSequence src, Pattern ptn, BiIntFunction<Matcher, CharSequence> replacer) {
        int idx = 0, last = 0, len = src.length();
        Matcher m = ptn.matcher(src);
        StringBuilder sb = new StringBuilder();

        // 扫描一次字符串
        while (m.find()) {
            sb.append(src, last, m.start()).append(replacer.apply(m, idx++));
            last = m.end();
        }
        // 如果表达式没有匹配或者匹配未到末尾，该判断保证字符串完整性
        if (last < len) {
            sb.append(src, last, len);
        }

        return sb;
    }

    /**
     * 获取SQL PARAMS字符串
     */
    public static String sqlParam(Object obj) {
        String repStr;
        if (obj instanceof Collection) {
            repStr = StringUtils.quotaMarkList((Collection<?>) obj);
        } else {
            repStr = StringUtils.quotaMark(obj);
        }
        return repStr;
    }

    /**
     * 使用单引号包含字符串
     *
     * @param obj 原字符串
     * @return 单引号包含的原字符串
     */
    public static String quotaMark(Object obj) {
        String srcStr = String.valueOf(obj);
        if (obj instanceof CharSequence) {
            // fix #79
            return StringEscape.escapeString(srcStr);
        }
        return srcStr;
    }

    /**
     * 使用单引号包含字符串
     *
     * @param coll 集合
     * @return 单引号包含的原字符串的集合形式
     */
    public static String quotaMarkList(Collection<?> coll) {
        return coll.stream().map(StringUtils::quotaMark)
                .collect(joining(StringPool.COMMA, StringPool.LEFT_BRACKET, StringPool.RIGHT_BRACKET));
    }

    /**
     * 拼接字符串第二个字符串第一个字母大写
     */
    public static String concatCapitalize(String concatStr, final String str) {
        if (isBlank(concatStr)) {
            concatStr = StringPool.EMPTY;
        }
        if (str == null || str.isEmpty()) {
            return str;
        }

        final char firstChar = str.charAt(0);
        if (Character.isTitleCase(firstChar)) {
            // already capitalized
            return str;
        }

        return concatStr + Character.toTitleCase(firstChar) + str.substring(1);
    }

    /**
     * 判断对象是否不为空
     *
     * @param object ignore
     * @return ignore
     */
    public static boolean checkValNotNull(Object object) {
        if (object instanceof CharSequence) {
            return isNotEmpty((CharSequence) object);
        }
        return object != null;
    }

    /**
     * 判断对象是否为空
     *
     * @param object ignore
     * @return ignore
     */
    public static boolean checkValNull(Object object) {
        return !checkValNotNull(object);
    }

    /**
     * 包含大写字母
     *
     * @param word 待判断字符串
     * @return ignore
     */
    public static boolean containsUpperCase(String word) {
        for (int i = 0; i < word.length(); i++) {
            char c = word.charAt(i);
            if (Character.isUpperCase(c)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 是否为大写命名
     *
     * @param word 待判断字符串
     * @return ignore
     */
    public static boolean isCapitalMode(String word) {
        return null != word && CAPITAL_MODE.matcher(word).matches();
    }

    /**
     * 是否为驼峰下划线混合命名
     *
     * @param word 待判断字符串
     * @return ignore
     */
    public static boolean isMixedMode(String word) {
        return matches(".*[A-Z]+.*", word) && matches(".*[/_]+.*", word);
    }

    /**
     * 判断是否以某个字符串结尾（区分大小写）
     * Check if a String ends with a specified suffix.
     * <p>
     * <code>null</code>s are handled without exceptions. Two <code>null</code>
     * references are considered to be equal. The comparison is case sensitive.
     * </p>
     * <p>
     * <pre>
     * StringUtils.endsWith(null, null)      = true
     * StringUtils.endsWith(null, "abcdef")  = false
     * StringUtils.endsWith("def", null)     = false
     * StringUtils.endsWith("def", "abcdef") = true
     * StringUtils.endsWith("def", "ABCDEF") = false
     * </pre>
     * </p>
     *
     * @param str    the String to check, may be null
     * @param suffix the suffix to find, may be null
     * @return <code>true</code> if the String ends with the suffix, case
     * sensitive, or both <code>null</code>
     * @see String#endsWith(String)
     * @since 2.4
     */
    public static boolean endsWith(String str, String suffix) {
        return endsWith(str, suffix, false);
    }

    /**
     * Check if a String ends with a specified suffix (optionally case
     * insensitive).
     *
     * @param str        the String to check, may be null
     * @param suffix     the suffix to find, may be null
     * @param ignoreCase inidicates whether the compare should ignore case (case-     *                   insensitive) or not.
     * @return <code>true</code> if the String starts with the prefix or both
     * <code>null</code>
     * @see String#endsWith(String)
     */
    private static boolean endsWith(String str, String suffix, boolean ignoreCase) {
        if (str == null || suffix == null) {
            return (str == null && suffix == null);
        }
        if (suffix.length() > str.length()) {
            return false;
        }
        int strOffset = str.length() - suffix.length();
        return str.regionMatches(ignoreCase, strOffset, suffix, 0, suffix.length());
    }

    /**
     * 是否为CharSequence类型
     *
     * @param clazz class
     * @return true 为是 CharSequence 类型
     */
    public static boolean isCharSequence(Class<?> clazz) {
        return clazz != null && CharSequence.class.isAssignableFrom(clazz);
    }

    /**
     * 前n个首字母小写,之后字符大小写的不变
     *
     * @param rawString 需要处理的字符串
     * @param index     多少个字符(从左至右)
     * @return ignore
     */
    public static String prefixToLower(String rawString, int index) {
        String field = rawString.substring(0, index).toLowerCase() +
                rawString.substring(index);
        return field;
    }

    /**
     * 删除字符前缀之后,首字母小写,之后字符大小写的不变
     * <p>StringUtils.removePrefixAfterPrefixToLower( "isUser", 2 )     = user</p>
     * <p>StringUtils.removePrefixAfterPrefixToLower( "isUserInfo", 2 ) = userInfo</p>
     *
     * @param rawString 需要处理的字符串
     * @param index     删除多少个字符(从左至右)
     * @return ignore
     */
    public static String removePrefixAfterPrefixToLower(String rawString, int index) {
        return prefixToLower(rawString.substring(index), 1);
    }

    /**
     * 驼峰转连字符
     * <p>StringUtils.camelToHyphen( "managerAdminUserService" ) = manager-admin-user-service</p>
     *
     * @param input ignore
     * @return 以'-'分隔
     * @see <a href="https://github.com/krasa/StringManipulation">document</a>
     */
    public static String camelToHyphen(String input) {
        return wordsToHyphenCase(wordsAndHyphenAndCamelToConstantCase(input));
    }

    private static String wordsAndHyphenAndCamelToConstantCase(String input) {
        StringBuilder buf = new StringBuilder();
        char previousChar = ' ';
        char[] chars = input.toCharArray();
        for (char c : chars) {
            boolean isUpperCaseAndPreviousIsLowerCase = (Character.isLowerCase(previousChar)) && (Character.isUpperCase(c));

            boolean previousIsWhitespace = Character.isWhitespace(previousChar);
            boolean lastOneIsNotUnderscore = (buf.length() > 0) && (buf.charAt(buf.length() - 1) != '_');
            boolean isNotUnderscore = c != '_';
            if (lastOneIsNotUnderscore && (isUpperCaseAndPreviousIsLowerCase || previousIsWhitespace)) {
                buf.append(StringPool.UNDERSCORE);
            } else if ((Character.isDigit(previousChar) && Character.isLetter(c))) {
                buf.append(UNDERLINE);
            }
            if ((shouldReplace(c)) && (lastOneIsNotUnderscore)) {
                buf.append(UNDERLINE);
            } else if (!Character.isWhitespace(c) && (isNotUnderscore || lastOneIsNotUnderscore)) {
                buf.append(Character.toUpperCase(c));
            }
            previousChar = c;
        }
        if (Character.isWhitespace(previousChar)) {
            buf.append(StringPool.UNDERSCORE);
        }
        return buf.toString();
    }

    private static boolean shouldReplace(char c) {
        return (c == '.') || (c == '_') || (c == '-');
    }

    private static String wordsToHyphenCase(String s) {
        StringBuilder buf = new StringBuilder();
        char lastChar = 'a';
        for (char c : s.toCharArray()) {
            if ((Character.isWhitespace(lastChar)) && (!Character.isWhitespace(c))
                    && ('-' != c) && (buf.length() > 0)
                    && (buf.charAt(buf.length() - 1) != '-')) {
                buf.append(StringPool.DASH);
            }
            if ('_' == c) {
                buf.append(StringPool.DASH);
            } else if ('.' == c) {
                buf.append(StringPool.DASH);
            } else if (!Character.isWhitespace(c)) {
                buf.append(Character.toLowerCase(c));
            }
            lastChar = c;
        }
        if (Character.isWhitespace(lastChar)) {
            buf.append(StringPool.DASH);
        }
        return buf.toString();
    }

    /**
     * <p>比较两个字符串，相同则返回true。字符串可为null</p>
     *
     * <p>对字符串大小写敏感</p>
     *
     * <pre>
     * StringUtils.equals(null, null)   = true
     * StringUtils.equals(null, "abc")  = false
     * StringUtils.equals("abc", null)  = false
     * StringUtils.equals("abc", "abc") = true
     * StringUtils.equals("abc", "ABC") = false
     * </pre>
     *
     * @param cs1 第一个字符串, 可为 {@code null}
     * @param cs2 第二个字符串, 可为 {@code null}
     * @return {@code true} 如果两个字符串相同, 或者都为 {@code null}
     * @see Object#equals(Object)
     */
    public static boolean equals(final CharSequence cs1, final CharSequence cs2) {
        if (cs1 == cs2) {
            return true;
        }
        if (cs1 == null || cs2 == null) {
            return false;
        }
        if (cs1.length() != cs2.length()) {
            return false;
        }
        if (cs1 instanceof String && cs2 instanceof String) {
            return cs1.equals(cs2);
        }
        // Step-wise comparison
        final int length = cs1.length();
        for (int i = 0; i < length; i++) {
            if (cs1.charAt(i) != cs2.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * SQL 注入字符串去除空白内容：
     * <ul>
     *     <li>\n 回车</li>
     *     <li>\t 水平制表符</li>
     *     <li>\s 空格</li>
     *     <li>\r 换行</li>
     * </ul>
     *
     * @param str 字符串
     */
    public static String sqlInjectionReplaceBlank(String str) {
        if (SqlInjectionUtils.check(str)) {
            /*
              存在 SQL 注入，去除空白内容
             */
            Matcher matcher = REPLACE_BLANK.matcher(str);
            return matcher.replaceAll("");
        }
        return str;
    }
}
