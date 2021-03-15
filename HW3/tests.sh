for j in "Null" "Synchronized" "Unsynchronized" "AcmeSafe"
do
    echo "$j : " &>> log11.txt
    for i in 5 69 100
    do
        for k in 1 8 25 40
        do
            echo "$k threads, $i array size test:" &>> log11.txt
            (time timeout 3600 java UnsafeMemory $j $k 100000000 $i) &>> log11.txt
            echo " " >> log11.txt
        done
    done
done
