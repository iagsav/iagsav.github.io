1. Python - feature_extractor.py

функция prepare_data: первоначанльные данные считываются, нормализуются далее нормализованный сигнал каждого подшипника пишется в отдельную папку на диске. Папка первоначальных данных задачется переменной RAW_DATA_FOLDER, а папка куда раскидывются нормализованные сигналы DATA_FOLDER.

функция extract_features: считывается с диска нормализованный сигнал из папки DATA_FOLDER. Далее каждый файл разбивается на кол-во сегментов регулируемое переменной CHUNK_TO_FILE_RATIO. Для каждого из сегментов считаются спектральные коэффициенты. Результат для каждого из подшипников кладется в папку UFORM_OUTPUT_FOLDER.

2. Java - Main.java

Спектральные коэффициенты считываются из папки, настраиваемой переменной BASE_DIR. Для каждого из спектральных коэффициентов рассчитывается СКО и результаты записываются с файл, указанный в переменной OUTPUT_FILE_NAME.

3 Python - sko4channels.py

массивы СКО считываются из файла, указанного впеременной SOURCE_FILE, на TRAIN_SET_SIZE первых значений обучается One-Class SVM, выполняется поиск аномалий, для аномалий считается кумулятивное СКО, результат хранится в массиве aggr_sko.

4. Python -uform.py

скрипт с функциями для выполнения U-преобразования (функция uform), поиска образов полных групп (analyze_full), остальные функции вспомогательные

5. Python - dataprovider.py

функции для чтения и записи на диск различных данных.

6. В остальных файлах либо мелкие вспомогательные функции либо тестирование чего-либо.