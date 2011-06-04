
// #include <windows.h>
// #include <windowsx.h>
// #include <olectl.h>
// #include <ddraw.h>
// 
// #include <wmsdk.h>

#include <atlbase.h>
#include <stdio.h>

#include <dshowasf.h>
#include <DShow.h>
#include <sbe.h>

HRESULT CreateFilterGraph(IGraphBuilder **pGraph)
{
	HRESULT hr;

	if (!pGraph)
		return E_POINTER;

	hr = CoCreateInstance(CLSID_FilterGraph, // get the graph object
		NULL,
		CLSCTX_INPROC_SERVER,
		IID_IGraphBuilder,
		(void **) pGraph);

	if(FAILED(hr))
	{
		printf("CreateFilterGraph: Failed to create graph!  hr=0x%x\n", hr);
		*pGraph = NULL;
		return hr;
	}

	return S_OK;
}

int __cdecl main(int argc, char *argv[])
{

	// Initialize COM
	HRESULT hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
	if (FAILED(hr))
		return hr;

	// build filter graph
	CComPtr <IGraphBuilder> pGraph;
	hr = CreateFilterGraph(&pGraph);
	if(FAILED(hr))
	{
		printf("Couldn't create filter graph! hr=0x%x", hr);
		return hr;
	}

	// Add the Stream Buffer Source filter to the graph.
	CComPtr<IStreamBufferSource> pSource;
	hr = pSource.CoCreateInstance(CLSID_StreamBufferSource);
	CComQIPtr<IBaseFilter> pSourceF(pSource);
	hr = pGraph->AddFilter(pSourceF, L"SBESource");
	if (FAILED(hr))
	{
		printf("Add SBESource filter failed!", hr);
		return hr;
	}

	// pass the filename
	CComQIPtr<IFileSourceFilter> pFileSource(pSource);
	hr = pFileSource->Load(L"test.wtv", 0);
	if (FAILED(hr))
	{
		printf("EnableDefaultMode failed!", hr);
		return hr;
	}

	// list stream information
	CComQIPtr<ISBE2Crossbar> pBar(pSource);
	CComPtr<ISBE2EnumStream> pStream;
	SBE2_STREAM_DESC stInfo;
// 	hr = pBar->EnableDefaultMode(DEF_MODE_PROFILE);
// 	if(FAILED(hr))
// 	{
// 		printf("EnableDefaultMode failed!", hr);
// 		return hr;
// 	}
	hr = pBar->EnumStreams(&pStream);
	if (FAILED(hr))
	{
		printf("EnumStreams failed!", hr);
		return hr;
	}
	
	int num = 0;

	while (hr = pStream->Next(1, &stInfo, NULL), hr == S_OK)
	{
		int stId = stInfo.StreamId;
		printf("The stream id is %d\n", stId);
		num++;
	}
	printf("The stream num is %d", num);
#if 0
	// Render each output pin.
	CComPtr<IPin> pSourcePinOut;
	CComPtr<IEnumPins> pPinEnum;
	hr = pSourceF->EnumPins(&pPinEnum);
	while (hr = pPinEnum->Next(1, &pSourcePinOut, 0), hr == S_OK)
	{
		hr = pGraph->Render(pSourcePinOut);
		pSourcePinOut.Release();
	}
#endif

	CoUninitialize();
	return hr;
 return 0;
}