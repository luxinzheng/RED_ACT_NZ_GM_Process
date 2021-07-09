program main
	use DFLIB
	implicit none
	character(80) Station_Name (1000) ! 台站名称
	integer Station_Name_Length (1000) ! 台站名称长度
	integer Num_Stations ! 台站数量

	CALL Get_Station_Name_List(Station_Name,Num_Stations,Station_Name_Length) !获取台站名称等信息

	Call Translate_File(Station_Name,Num_Stations,Station_Name_Length) !把新西兰台站命名为日本台站格式

	CALL Generate_InputFile(Station_Name,Num_Stations,Station_Name_Length) ! 生成相应EW.txt, NS.TXT, UD.TXT 输入文件	
	
	call Copy_Files(Station_Name,Num_Stations,Station_Name_Length) ! 将输入文件拷贝到对应目录下

	call Station_List(Station_Name,Num_Stations,Station_Name_Length) ! 生成台站列表

	pause

	stop
end program 

subroutine Translate_File(Station_Name,Num_Stations,Station_Name_Length) !把新西兰台站命名为日本台站格式
	implicit none
	character(80) Station_Name (1000) ! 台站名称
	integer Station_Name_Length (1000) ! 台站名称长度
	integer Num_Stations
	
	integer I, J, L
	real*8 X1,X2,X3,X,Y1,Y2,Y3,Y
	integer N_Acc ! 加速度个数
	character(100) A,B
	character(80) Temp_Name
	real*8 EW(100000), NS(100000), UD(100000)
	integer N_Acc_100Hz ! 压缩到100Hz间隔
	integer EW100HZ(100000), NS100HZ(100000), UD100HZ(100000)


	open (66,file="StationList.txt")

	do I=1, Num_Stations
		Temp_Name=" "
		L=Station_Name_Length (I)
		Temp_Name(1:L)=Station_Name(I)(1:L)
		Temp_Name(L+1:L+4)=".v1a"; 		open(55,file=trim(Temp_Name))
		Temp_Name(L+1:L+4)=".EW "; 		open(56,file=trim(Temp_Name)) !EW
		Temp_Name(L+1:L+4)=".NS "; 		open(57,file=trim(Temp_Name)) !NS
		Temp_Name(L+1:L+4)=".UD "; 		open(58,file=trim(Temp_Name)) !NS
	
		read(55,*) ! Line 1
		read(55,"(A80)") A ! Line 2
		read(A(16:17) ,"(I2)") J; Y1=J;
		read(A(19:20) ,"(I2)") J; Y2=J;
		read(A(22:23) ,"(I2)") J; Y3=J;
		read(A(27:29) ,"(I3)") J; X1=J;
		read(A(31:32) ,"(I2)") J; X2=J;
		read(A(34:35) ,"(I2)") J; X3=J;
		X=X1+X2/60.+X3/60./60.
		Y=-(Y1+Y2/60.+Y3/60./60.)

		read(55,*); read(55,*);read(55,*);read(55,*);read(55,*);read(55,*);read(55,*) !3456789
		read(55,"(A80)") A ! Line 10
		B(1:8)=A(17:24)
		read(B(1:8), "(I8)") N_Acc
		read(55,*); read(55,*); read(55,*); read(55,*); read(55,*); read(55,*); !11-16
		read(55,*); read(55,*); read(55,*); read(55,*); read(55,*); read(55,*); !17-22
		read(55,*); read(55,*); read(55,*); read(55,*); !22-26
		!read(55,"(A80)") A ! Line 10

		read(55,*) EW(1:N_Acc)

		do J=1, 26; read(55,*); end do ! NS
		read(55,*) NS(1:N_Acc)
		do J=1, 26; read(55,*); end do ! UD
		read(55,*) UD(1:N_Acc)
		
		N_Acc_100Hz=N_Acc/2
		do J=1,N_Acc_100Hz
			EW100HZ(J)=int(EW((J-1)*2+1)*1.) 
			NS100HZ(J)=int(NS((J-1)*2+1)*1.) 
			UD100HZ(J)=int(UD((J-1)*2+1)*1.) 
		end do

		write(56,"(A18)") "Origin Time       "
		write(56,"(A18)") "Lat.              "
		write(56,"(A18)") "Long.             "
		write(56,"(A18)") "Depth. (km)       "
		write(56,"(A18)") "Mag.              "
		write(56,"(A18, A4)") "Station Code      ", trim(Station_Name(I)(17:20))
		write(56,"(A18,F9.5)") "Station Lat.      ",Y !34.8158
		write(56,"(A18,F9.5)") "Station Long.     ",X !139.0546
		write(56,"(A23)") "Station Height(m) 149.3"
		write(56,"(A37)") "Record Time       2019/02/08 06:34:30"
		write(56,"(A23)") "Sampling Freq(Hz) 100Hz"
		write(56,"(A18,F7.3)") "Duration Time(s)  ",N_Acc_100Hz*0.01
		write(56,"(A21)") "Dir.              E-W"
		write(56,"(A30)") "Scale Factor       10(gal)/100"
		write(56,"(A18,F10.3)") "Max. Acc. (gal)   ",maxval(abs(EW100HZ))/10.
		write(56,"(A37)") "Last Correction   2019/02/08 06:34:15"
		write(56,"(A5)") "Memo.  "
		write(56,"(8I9)") int(EW100Hz(1:N_Acc_100Hz))

!20190205_040511_TDHS_20.EW

		write(57,"(A18)") "Origin Time       "
		write(57,"(A18)") "Lat.              "
		write(57,"(A18)") "Long.             "
		write(57,"(A18)") "Depth. (km)       "
		write(57,"(A18)") "Mag.              "
		write(57,"(A18, A4)") "Station Code      ", trim(Station_Name(I)(17:20))
		write(57,"(A18,F9.5)") "Station Lat.      ",Y !34.8158
		write(57,"(A18,F9.5)") "Station Long.     ",X !139.0546
		write(57,"(A23)") "Station Height(m) 149.3"
		write(57,"(A37)") "Record Time       2019/02/08 06:34:30"
		write(57,"(A23)") "Sampling Freq(Hz) 100Hz"
		write(57,"(A18,F7.3)") "Duration Time(s)  ",N_Acc_100Hz*0.01
		write(57,"(A21)") "Dir.              N-S"
		write(57,"(A30)") "Scale Factor       10(gal)/100"
		write(57,"(A18,F10.3)") "Max. Acc. (gal)   ",maxval(abs(NS100HZ))/10.
		write(57,"(A37)") "Last Correction   2019/02/08 06:34:15"
		write(57,"(A5)") "Memo.  "
		write(57,"(8I9)") int(NS100Hz(1:N_Acc_100Hz))

		write(58,"(A18)") "Origin Time       "
		write(58,"(A18)") "Lat.              "
		write(58,"(A18)") "Long.             "
		write(58,"(A18)") "Depth. (km)       "
		write(58,"(A18)") "Mag.              "
		write(58,"(A18, A4)") "Station Code      ", trim(Station_Name(I)(17:20))
		write(58,"(A18,F9.5)") "Station Lat.      ",Y !34.8158
		write(58,"(A18,F9.5)") "Station Long.     ",X !139.0546
		write(58,"(A23)") "Station Height(m) 149.3"
		write(58,"(A37)") "Record Time       2019/02/08 06:34:30"
		write(58,"(A23)") "Sampling Freq(Hz) 100Hz"
		write(58,"(A18,F7.3)") "Duration Time(s)  ",N_Acc_100Hz*0.01
		write(58,"(A21)") "Dir.              U-D"
		write(58,"(A30)") "Scale Factor       10(gal)/100"
		write(58,"(A18,F10.3)") "Max. Acc. (gal)   ",maxval(abs(UD100HZ))/10.
		write(58,"(A37)") "Last Correction   2019/02/08 06:34:15"
		write(58,"(A5)") "Memo.  "
		write(58,"(8I9)") int(UD100Hz(1:N_Acc_100Hz))

		close(58)
		close(57)
		close(56)
		close(55)
		

		write(66,"(I5, A40, 2F12.5)") I, Station_Name(I)(1:Station_Name_Length(I)), X, Y ! 只保留名称和坐标信息

	end do

	close(66)

	return
end subroutine

subroutine Copy_Files(Station_Name,Num_Stations,Station_Name_Length) !将输入文件拷贝到对应目录下
	USE DFLIB
	implicit none
	character(80) Station_Name (1000) ! 台站名称
	integer Station_Name_Length (1000) ! 台站名称长度
	integer Num_Stations

	integer I, L, M , II !临时文件
	character(80) Temp_Name !临时文件
	character(80) CMD !临时文件

	do I=1, Num_Stations 
	   WRITE (*,"(1X,A40)") Station_Name(I) !输出台站名称
	   CMD=" "
	   CMD="MD "//Station_Name(I) 
	   II=SYSTEMQQ(CMD) ! 建立文件夹
	end do

	do I=1, Num_Stations ! 把文件拷贝到对应文件夹下 "copy ABC.EW.TXT  .\ABC\EW.TXT"
		CMD= ' '
		L=Station_Name_Length(I)
		CMD(1:5)="move"
		M=5
		CMD(M+1:M+L)=Station_Name(I)(1:L)
		M=M+L
		CMD(M+1:M+11)=".EW.TXT  .\"
		M=M+11
		CMD(M+1:M+L)=Station_Name(I)(1:L)
		M=M+L
		CMD(M+1:M+8)="\EW.TXT"
		!write(*,*) CMD 
		II=SYSTEMQQ(CMD)
		
		CMD(1:5)="move"
		M=5
		CMD(M+1:M+L)=Station_Name(I)(1:L)
		M=M+L
		CMD(M+1:M+11)=".NS.TXT  .\"
		M=M+11
		CMD(M+1:M+L)=Station_Name(I)(1:L)
		M=M+L
		CMD(M+1:M+8)="\NS.TXT"
		!write(*,*) CMD 
		II=SYSTEMQQ(CMD)

		CMD(1:5)="move"
		M=5
		CMD(M+1:M+L)=Station_Name(I)(1:L)
		M=M+L
		CMD(M+1:M+11)=".UD.TXT  .\"
		M=M+11
		CMD(M+1:M+L)=Station_Name(I)(1:L)
		M=M+L
		CMD(M+1:M+8)="\UD.TXT"
		!write(*,*) CMD 
		II=SYSTEMQQ(CMD)

	end do

	II=SYSTEMQQ("del *.EW")
	II=SYSTEMQQ("del *.NS")
	II=SYSTEMQQ("del *.UD")

	return
end subroutine 

subroutine Generate_InputFile(Station_Name,Num_Stations,Station_Name_Length) ! 生成相应EW.txt, NS.TXT, UD.TXT 输入文件	
	USE DFLIB
	character(80) Station_Name (1000) ! 台站名称
	integer Station_Name_Length (1000) ! 台站名称长度
	integer Num_Stations

	character(80) Temp_Name
	character(5) F
	integer L
		
	open(55,file="input.txt")
	write(55,*) Num_Stations*3 ! 总记录数
	do I=1, Num_Stations		
		L=Station_Name_Length(I) ! 台站名称长度
		F="(A25)"
		write(F(3:4),"(I2)") L+3 ! 得到精确字符串表达
		Temp_Name(1:L)=Station_Name(I)(1:L)
		Temp_Name(L+1:L+3)=".EW"; write(55,F) Temp_Name		
		Temp_Name(L+1:L+3)=".NS"; write(55,F) Temp_Name
		Temp_Name(L+1:L+3)=".UD"; write(55,F) Temp_Name		
	end do
	JJ=systemQQ("ProcessGM_Japan.exe") ! 调用地震动处理程序

	return
end subroutine Generate_InputFile

subroutine Station_List(Station_Name,Num_Stations,Station_Name_Length) ! 生成台站列表, 为生成网页服务
	USE DFLIB
	implicit none
	character(80) Station_Name (1000) ! 台站名称
	integer Station_Name_Length (1000) ! 台站名称长度
	integer Num_Stations
	
	integer I	

	real*8 X(2)
	character A

!	open (55, file="StationLocation.txt")
!	open (66,file="StationList.txt")
!		read(55,*)
!		do I=1, Num_Stations
!			read (55,*) A, X(1), X(2)
!			read (55,*)
!			read (55,*)
!			write(66,"(I5, A40, 2F12.5)") I, Station_Name(I)(1:Station_Name_Length(I)), X(1:2) ! 只保留名称和坐标信息
!		end do
!	close (55)
!	close(66)

	return
end subroutine

!  SUBROUTINE to demonstrate GETFILEINFOQQ
SUBROUTINE Get_Station_Name_List(Station_Name,Num_Stations,Station_Name_Length) !获取台站名称等信息
	USE DFLIB
	character(80) Station_Name (1000) ! 台站名称
	integer Station_Name_Length (1000) ! 台站名称长度
	integer Num_Stations
	CHARACTER(80)    files
	INTEGER(4)       handle, length
	CHARACTER(5)     permit
	TYPE (FILE$INFO) info
	character(80) Temp_Name

	Num_Stations=0

	files="*.v1a"
	handle = FILE$FIRST	

	DO WHILE (.TRUE.)
		length = GETFILEINFOQQ(files, info, handle)
		IF ((handle .EQ. FILE$LAST) .OR. &
			(handle .EQ. FILE$ERROR)) THEN
		SELECT CASE (GETLASTERRORQQ( ))
			CASE (ERR$NOMEM)
				WRITE (*,*) 'Out of memory'
			CASE (ERR$NOENT)
				EXIT
			CASE DEFAULT
				WRITE (*,*) 'Invalid file or path name'
		END SELECT
		END IF
   permit = ' '
   !IF ((info%permit .AND. FILE$ARCHIVE) .NE. 0) then
		Num_Stations=Num_Stations+1
		Temp_Name=info%name
		do I=len(Temp_Name),1,-1
			if(Temp_Name(I:I)==".") then
				Station_Name(Num_Stations)(1:I-1)=Temp_Name(1:I-1)
				Station_Name_Length(Num_Stations)=I-1
			end if
		end do
   !end if
 END DO
 END SUBROUTINE



	
