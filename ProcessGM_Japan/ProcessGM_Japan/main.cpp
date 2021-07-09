#include <iostream>
#include <windows.h>
#include <sstream>
#include <fstream>
#include<string>
#include <math.h> 
#include<vector>
#include <cassert>  
#include <stack> 
#define PI 3.1415927
using namespace std;

void getGM_Japan(string filename, vector<double>&EQ, vector<double>&GM_para, int flag, string &stationName);
vector< string> split(string str, string pattern);
//template<class T>
//int length(T& data)
//{
//	return sizeof(data) / sizeof(data[0]);
//}
int main()
{
	string filenameX1, filenameY, filenameX2;
	int N = 0;
	int num = 1;//读取地震动数量
	double dt = 0.01;
	int flag = 2;
	ifstream ipt("input.txt");
	double longitude, latitude, PGA1, PGA2, PGA;//经度纬度
	ofstream opt2("StationLocation.txt");
	opt2 << "台站名称\t经度\t纬度\tPGA\t时间间隔\t总步数" << "\n";
	ipt >> num;
	//vector<vector<double>> EQ(num);//定义3XZZZ的数组
	//vector<vector<double>> Sa(num);//定义3XZZZ的数组
	//num = int(num / 2.0);
	for (int i = 0; i < num; i++)
	{
		vector<double>EQX1;
		//vector<double>EQX2;
		vector<double>GM_para1;//用来存储总步数和步长
		//vector<double>GM_para2;//用来存储总步数和步长
		double t = 0;
		string Name;
		ipt >> filenameX1 ;
		getGM_Japan(filenameX1, EQX1, GM_para1, flag, Name);
		//getGM_Japan(filenameX2, EQX2, GM_para2, flag, Name);
		//filenameX.erase(filenameX.end() - 3, filenameX.end());

		vector<double>::iterator it;//声明一个迭代器，来访问vector容器，作用：遍历或者指向vector容器的元素 
		N = int(GM_para1[0]);
		dt = GM_para1[1];
		latitude = GM_para1[2];
		longitude = GM_para1[3];
		PGA = GM_para1[4];
		//PGA2 = GM_para2[4];

		filenameY = filenameX1 + ".txt";
		ofstream opt(filenameY);
		opt << N << "\n";
		t = -dt;
		for (int j = 0; j < N; j++)
		{
			t = t + dt;
			opt << t << "\t" << EQX1[j]-EQX1[0] << "\n";
		}
		opt2 << Name << "\t" << longitude << "\t" << latitude << "\t" << PGA << "\t" << dt << "\t" << N << "\n";
	}
	return 0;
}
void getGM_Japan(string filename, vector<double>&EQ, vector<double>&GM_para, int flag, string &stationName) {
	double dt, PGA;
	int N;
	ifstream ipt(filename);
	double Lat, Long;//读取地震动经纬度 纬度 经度
	double Freq;
	string FreqString;
	//string stationName;
	string temp;
	double ScaleFactor, duration;
	string ScaleFactorString;
	double tempD;
	for (int i = 0; i < 5; i++)
	{
		getline(ipt, temp);
	}
	ipt >> temp >> temp >> stationName;
	ipt >> temp >> temp >> Lat;
	ipt >> temp >> temp >> Long;
	for (int i = 0; i < 3; i++)
	{
		getline(ipt, temp);
	}
	ipt >> temp >> temp >> FreqString;
	FreqString.erase(FreqString.end() - 2, FreqString.end());
	Freq = atof(FreqString.c_str());
	dt = 1 / Freq;
	ipt >> temp >> temp >> duration;
	N = int(duration / dt);
	getline(ipt, temp);
	getline(ipt, temp);
	ipt >> temp >> temp >> ScaleFactorString;
	string pattern = "(gal)/";
	vector< string> result = split(ScaleFactorString, pattern);
	ScaleFactor = atof(result[0].c_str()) / atof(result[1].c_str());
	ipt >> temp >> temp >> temp >> PGA;
	for (int i = 0; i < 3; i++)
	{
		getline(ipt, temp);
	}
	for (int i = 0; i < N; i++)
	{
		ipt >> tempD;
		EQ.push_back(tempD*ScaleFactor / 100.0);
	}
	GM_para.push_back(N);
	GM_para.push_back(dt);
	GM_para.push_back(Lat);
	GM_para.push_back(Long);
	GM_para.push_back(PGA / 100.0);
}

vector< string> split(string str, string pattern)
{
	vector<string> ret;
	if (pattern.empty()) return ret;
	size_t start = 0, index = str.find_first_of(pattern, 0);
	while (index != str.npos)
	{
		if (start != index)
			ret.push_back(str.substr(start, index - start));
		start = index + 1;
		index = str.find_first_of(pattern, start);
	}
	if (!str.substr(start).empty())
		ret.push_back(str.substr(start));
	return ret;
}

