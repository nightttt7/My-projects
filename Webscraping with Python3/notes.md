# 1. 环境搭建
- 环境配置清单(pass: 无需安装/很熟悉暂不配置; pxx: 位于xx页,暂不配置): 
    - windows7, linux-centos, python2&3, jupyter lab
    - requests(pip), selenium(pip)
    - chrome(win&centos: install autotrace,libuninameslist => fontforge => liberation-fonts =>  chrome **using local rpm**), chromedriver(win&centos)
    - PhantomJS(win&centos)
    - aiohttp, lxml, beautifulsoup4, pyquery 
    - tesserocr, tesseract(p23)
    - database(pass)
    - 存储库(pass)
    - web库(pass)
    - APP相关(p43)
    - pyspider(p59), scrapy, scrapy-splash(p65), scrapy-redis
    - docker(pass), scrapyd(p71), scrapyd-(p74)
# 2. 爬虫基础
    1. F12-network: http请求过程
    2. content-type, response status code p84-p85
    3. http&网页基础
    3. 爬虫基本原理
# 3. 基本库的使用
    1. urllib
    - ch3-urllib
    - User-agent的历史: IE想要伪装成Netscape 4; Konqueror 和 WebKit 想要伪装成 Firefox; Chrome想要伪装成Safari。
    - urllib
    - urllib.request.BaseHandler
    - error
    - 解析
        - urlparse等, 用于url处理
    - robots协议
    2. requests
    - requests
    - 正则(pass)
    3. 猫眼电影抓取
    - F12=>Network=>监听组件中查看源代码
    - 不要在Elements选项卡中直接查看源码，因为那里的源码可能经过JavaScript操作而与原始请求不同，而是需要从Network选项卡部分查看原始请求得到的源码。
# 4. 解析库
    1. xpath (lxml库)
    - / 直接子节点
    - // 子孙节点
    - /.. 父节点
    - @[] 属性
    - 选取出来的都是 list
    - 中文出现编码错误, 来源于lxml库的内部bug!!
    2. beautiful soup (bs4库)
    - 节点选择器
        - children 直接子节点
        - descendants 子孙节点
        - parent(s) 父节点/祖先节点
        - next_sibling(s) previous_sibling(s) 兄弟节点
    - 方法选择器,CSS选择器
    3. pyquery(JQuery风格 pass)
# 5. 数据存储 (pass)
# 6. Ajax
    - Ajax:Asynchronous JavaScript and XML
    - python模拟ajax请求
    - 实例:微博:六小龄童
    - 实战:今日头条街拍美图
# 7. 动态渲染
    - **selenium**
    - from selenium import webdriver
    - 如果页面中还有子Frame，它是不能获取到子Frame里面的节点的. 这时就需要使用browser.switch_to.frame()方法来切换Frame.
    - browser.switch_to.parent_frame()
    - splash (pass)
    - 使用selenium爬取淘宝商品 (pass)
# 8. 验证码识别 (pass)
# 9. 代理
    - unfinished
# 10. 模拟登录
    - unfinished
# 11. App爬取 (pass)
# 12. pyspider (pass)
# 13. scrapy
    - unfinished
# 14. 分布式爬虫
    - unfinished
# 15. 分布式爬虫的部署
    - unfinished
