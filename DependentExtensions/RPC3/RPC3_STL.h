/*
 *  Copyright (c) 2014, Oculus VR, Inc.
 *  All rights reserved.
 *
 *  This source code is licensed under the BSD-style license found in the
 *  LICENSE file in the root directory of this source tree. An additional grant
 *  of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#ifndef __RPC3_BOOST_H
#define __RPC3_BOOST_H

#include <type_traits>
#include <functional>
#include <tuple>
#include <iterator>



/*INVOKE FUNCTIONS START
TODO: Put these in to own generic file.
*/
template <class F, class... Args>
auto INVOKE(F&& f, Args&&... args) ->
    decltype(std::forward<F>(f)(std::forward<Args>(args)...)) {
        return std::forward<F>(f)(std::forward<Args>(args)...);
}

template <class F>
auto INVOKE(F&& f) ->
    decltype(std::forward<F>(f)()) {
        return std::forward<F>(f)();
}


namespace detail {
    template <typename F, typename Tuple, bool Done, int Total, int... N>
    struct call_impl {
        static auto call(F && f, Tuple && t) {
            return call_impl<F, Tuple, Total == 1 + sizeof...(N), Total,
                        N..., sizeof...(N)>::call(f, std::forward<Tuple>(t));
        }
    };

    template <typename F, typename Tuple, int Total, int... N>
    struct call_impl<F, Tuple, true, Total, N...> {
        static auto call(F && f, Tuple && t) {
            return std::forward<F>(f)(std::get<N>(std::forward<Tuple>(t))...);
        }
    };
    
    
    template <typename F, typename Tuple>
    struct empty_call_impl {
        static void call(F && f, Tuple && t) {
        }
    };
}

template <class F, class Tuple>
auto INVOKE(F&& f, Tuple&& t) {
    typedef typename std::decay<Tuple>::type ttype;
    return detail::call_impl<F, Tuple, 0 == std::tuple_size<ttype>::value,
                std::tuple_size<ttype>::value>::call(f, std::forward<Tuple>(t));
}

/*INVOKE FUNCTIONS END*/

// Fixes
// error C2504: 'boost::fusion::detail::invoke_impl<Function,Sequence,N,CBI,RandomAccess>' : base class undefined
// This defines the maximum number of parameters you can have
//TODO: There should be no maximum anymore.
#ifndef BOOST_FUSION_INVOKE_MAX_ARITY
#define BOOST_FUSION_INVOKE_MAX_ARITY 10
#endif


/*
// Boost dependencies
// Boost is assumed to be at C:\boost_1_43_0 based on the project settings
// If this is not where you downloaded boost, change the project settings Configuration Properties / C/C++ / General / Additional Include Directories
// If you don't have boost, get it from http://www.boost.org/users/download/
// If you don't want to use boost, use RPC4 instead which relies on assembly but has fewer features
#include "boost/type_traits.hpp"
#include "boost/function.hpp"
#include "boost/bind.hpp"
#include "boost/mpl/if.hpp"
#include "boost/mpl/apply.hpp"
#include "boost/function_types/parameter_types.hpp"
#include "boost/fusion/container/list/cons.hpp" // boost::fusion::nil
#include "boost/fusion/include/push_back.hpp"
#include "boost/fusion/include/invoke.hpp"
#include "boost/fusion/tuple/tuple.hpp"
#include "boost/fusion/tuple/make_tuple.hpp"
#include "boost/fusion/functional/invocation/invoke.hpp"
#include "boost/type_traits/is_array.hpp"
*/
// Not needed?
//#include <boost/fusion/container/generation/make_vector.hpp>

#include "NetworkIDManager.h"
#include "NetworkIDObject.h"
#include "BitStream.h"

namespace RakNet
{
class RPC3;
class BitStream;


namespace _RPC3
{

enum InvokeResultCodes
{
	IRC_SUCCESS,
	IRC_NEED_BITSTREAM,
	IRC_NEED_NETWORK_ID_MANAGER,
	IRC_NEED_NETWORK_ID,
	IRC_NEED_CLASS_OBJECT,
};

struct InvokeArgs
{
	// Bitstream to use to deserialize
	RakNet::BitStream *bitStream;

	// NetworkIDManager to use to lookup objects
	NetworkIDManager *networkIDManager;

	// C++ class member object
	NetworkID classMemberObjectId;

	// The calling plugin
	RPC3 *caller;

	// The this pointer for C++
	NetworkIDObject *thisPtr;
};

typedef std::tuple<bool, std::function<InvokeResultCodes(InvokeArgs)> > FunctionPointer;

struct StrWithDestructor
{
	char *c;
	~StrWithDestructor() {if (c) delete c;}
};

enum RPC3TagFlag
{
	RPC3_TAG_FLAG_DEREF=1,
	RPC3_TAG_FLAG_ARRAY=2,
};

struct RPC3Tag
{
	RPC3Tag() {}
	RPC3Tag(void *_v, unsigned int _count, RPC3TagFlag _flag) : v(_v), count(_count), flag((unsigned char)_flag) {}
	void* v;
	unsigned int count;
	unsigned char flag;
};

// Track the pointers tagged with RakNet::_RPC3::Deref
static RPC3Tag __RPC3TagPtrs[BOOST_FUSION_INVOKE_MAX_ARITY+1];
static int __RPC3TagHead=0;
static int __RPC3TagTail=0;

// If this assert hits, then RakNet::_RPC3::Deref was called more times than the argument was passed to the function
static void __RPC3_Tag_AddHead(const RPC3Tag &p)
{
	// Update tag if already in array
	int i;
	for (i=__RPC3TagTail; i!=__RPC3TagHead; i=(i+1)%BOOST_FUSION_INVOKE_MAX_ARITY)
	{
		if (__RPC3TagPtrs[i].v==p.v)
		{
			if (p.flag==RPC3_TAG_FLAG_ARRAY)
			{
				__RPC3TagPtrs[i].count=p.count;
			}
			__RPC3TagPtrs[i].flag|=p.flag;

			return;
		}
	}

	__RPC3TagPtrs[__RPC3TagHead]=p;
	__RPC3TagHead = (__RPC3TagHead + 1) % BOOST_FUSION_INVOKE_MAX_ARITY;
	assert(__RPC3TagHead!=__RPC3TagTail);
}
static void __RPC3ClearTail(void) {
	while (__RPC3TagTail!=__RPC3TagHead)
	{
		if (__RPC3TagPtrs[__RPC3TagTail].v==0)
			__RPC3TagTail = (__RPC3TagTail+1) % BOOST_FUSION_INVOKE_MAX_ARITY;
		else
			return;
	}
}
static bool __RPC3ClearPtr(void* p, RPC3Tag *tag) {
	int i;
	for (i=__RPC3TagTail; i!=__RPC3TagHead; i=(i+1)%BOOST_FUSION_INVOKE_MAX_ARITY)
	{
		if (__RPC3TagPtrs[i].v==p)
		{
			*tag=__RPC3TagPtrs[i];
			__RPC3TagPtrs[i].v=0;
			__RPC3ClearTail();
			return true;
		}
	}
	tag->flag=0;
	tag->count=1;
	return false;
}

template <class templateType>
inline const templateType& Deref(const templateType & t) {
	__RPC3_Tag_AddHead(RPC3Tag((void*)t,1,RPC3_TAG_FLAG_DEREF));
	return t;
}

template <class templateType>
inline const templateType& PtrToArray(unsigned int count, const templateType & t) {
	__RPC3_Tag_AddHead(RPC3Tag((void*)t,count,RPC3_TAG_FLAG_ARRAY));
	return t;
}

struct ReadBitstream
{
	static void applyArray(RakNet::BitStream &bitStream, RakNet::BitStream* t){apply(bitStream,t);}

	static void apply(RakNet::BitStream &bitStream, RakNet::BitStream* t)
	{
		BitSize_t numBitsUsed;
		bitStream.ReadCompressed(numBitsUsed);
		bitStream.Read(t,numBitsUsed);
	}
};

//template <typename T>
struct ReadPtr
{
	template <typename T2>
	static inline void applyArray(RakNet::BitStream &bitStream, T2 *t) {bitStream >> (*t);}
	template <typename T2>
	static inline void apply(RakNet::BitStream &bitStream, T2 *t) {bitStream >> (*t);}

	static inline void apply(RakNet::BitStream &bitStream, char *&t) {applyStr(bitStream, (char *&) t);}
	static inline void apply(RakNet::BitStream &bitStream, unsigned char *&t) {applyStr(bitStream, (char *&) t);}
	static inline void apply(RakNet::BitStream &bitStream, const char *&t) {applyStr(bitStream, (char *&) t);}
	static inline void apply(RakNet::BitStream &bitStream, const unsigned char *&t) {applyStr(bitStream, (char *&) t);}
	static inline void applyStr(RakNet::BitStream &bitStream, char *&t)
	{
		RakNet::RakString rs;
		bitStream >> rs;
		size_t len = rs.GetLength()+1;
		
		// The caller should have already allocated memory, so we need to free
		// it and allocate a new buffer.
		RakAssert("Expected allocated array, got NULL" && (NULL != t));
		delete [] t;

		t = new char [len];
		memcpy(t,rs.C_String(),len);
	}
};


template< typename T >
struct DoRead
{
	typedef typename std::conditional<
		std::is_convertible<T*,RakNet::BitStream*>::value,
		ReadBitstream,
		ReadPtr >::type type;
};


template< typename T >
struct ReadWithoutNetworkIDNoPtr
{
	static InvokeResultCodes apply(InvokeArgs &args, T &t)
	{
//		printf("ReadWithoutNetworkIDNoPtr\n");

		DoRead< typename std::remove_pointer<T>::type >::type::apply(* (args.bitStream),&t);

		return IRC_SUCCESS;
	}

	// typedef boost::mpl::false_ Cleanup;
	template< typename T2 >
	static void Cleanup(T2 &t) {}
};

template< typename T >
struct ReadWithNetworkIDPtr
{
	static InvokeResultCodes apply(InvokeArgs &args, T &t)
	{
//		printf("ReadWithNetworkIDPtr\n");
		// Read the network ID

		bool isNull;
		args.bitStream->Read(isNull);
		if (isNull)
		{
			t=0;
			return IRC_SUCCESS;
		}

		bool deref, isArray;
		args.bitStream->Read(deref);
		args.bitStream->Read(isArray);
		unsigned int count;
		if (isArray)
			args.bitStream->ReadCompressed(count);
		else
			count=1;
		NetworkID networkId;
		for (unsigned int i=0; i < count; i++)
		{
			args.bitStream->Read(networkId);
			t = args.networkIDManager->GET_OBJECT_FROM_ID< T >(networkId);
			if (deref)
			{
				BitSize_t bitsUsed;
				args.bitStream->AlignReadToByteBoundary();
				args.bitStream->Read(bitsUsed);

				if (t)
				{
					DoRead< typename std::remove_pointer<T>::type >::type::apply(* (args.bitStream),t);
				}
				else
				{
					// Skip data!
					args.bitStream->IgnoreBits(bitsUsed);
				}
			}
		}
		
		return IRC_SUCCESS;
	}

	template< typename T2 >
	static void Cleanup(T2 &t) {}
};

template< typename T >
struct ReadWithoutNetworkIDPtr
{
	template <typename T2>
	static InvokeResultCodes apply(InvokeArgs &args, T2 &t)
	{
//		printf("ReadWithoutNetworkIDPtr\n");
		
		bool isNull=false;
		args.bitStream->Read(isNull);
		if (isNull)
		{
			t=0;
			return IRC_SUCCESS;
		}

		typedef typename std::remove_pointer< T >::type ActualObjectType;

		bool isArray=false;
		unsigned int count;
		args.bitStream->Read(isArray);
		if (isArray)
			args.bitStream->ReadCompressed(count);
		else
			count=1;

		t = new ActualObjectType[count]();
		if (isArray)
		{
			for (unsigned int i=0; i < count; i++)
			{
				DoRead< typename std::remove_pointer<T>::type >::type::applyArray(* (args.bitStream),t+i);
			}
		}
		else
		{
			DoRead< typename std::remove_pointer<T>::type >::type::apply(* (args.bitStream),t);
		}

		return IRC_SUCCESS;
	}

	template< typename T2 >
	static void Cleanup(T2 &t) {
		if (t)
			delete [] t;
	}
};

template< typename T >
struct SetRPC3Ptr
{
	static InvokeResultCodes apply(InvokeArgs &args, T &obj)
	{
		obj=args.caller;
		return IRC_SUCCESS;
	}

	//typedef boost::mpl::false_ Cleanup;
	template< typename T2 >
	static void Cleanup(T2 &t) {}
};

/*
template< typename T >
struct ReadWithNetworkID
{
	typedef typename boost::mpl::if_<
		boost::is_pointer<T>
		, typename ReadWithNetworkIDPtr<T> // true
		, typename ReadWithNetworkIDNoPtr<T>
	>::type type;
};
*/

template< typename T >
struct ReadWithoutNetworkID
{
	typedef typename std::conditional<
		std::is_pointer<T>::value
		, ReadWithoutNetworkIDPtr<T> // true
		, ReadWithoutNetworkIDNoPtr<T>
	>::type type;
};

template< typename T >
struct identity
{
	typedef T type;
};

/*template< typename T >
struct IsRPC3Ptr
{
	typedef typename std::conditional<
		std::is_convertible<T,RPC3*>::value,
		std::true_type,
		std::false_type>::type type;
};*/
/*
template< typename T >
struct ShouldReadNetworkID
{
	
	typedef typename boost::mpl::if_<
		boost::is_pointer<T>,
		typename identity<T>::type,
		boost::add_pointer<T>>::type typeWithPtr;

	typedef typename boost::mpl::if_<
		boost::is_convertible<typeWithPtr,NetworkIDObject*>,
		boost::mpl::true_,
		boost::mpl::false_>::type type;
		

	typedef typename std::conditional<
		std::is_convertible<T,NetworkIDObject*>::value,
		std::true_type::value_type,
		std::false_type::value_type>::type type;
};*/

template< typename T >
struct GetReadFunction
{
	/*
	typedef typename boost::mpl::if_<
		typename ShouldReadNetworkID<T>::type
		, typename ReadWithNetworkID<T>::type
		, typename ReadWithoutNetworkID<T>::type
	>::type type;
	*/
    //const bool
	typedef typename std::conditional<
		std::is_convertible<T, NetworkIDObject*>::value
		, ReadWithNetworkIDPtr<T>
		, typename ReadWithoutNetworkID<T>::type
	>::type type;
};

template< typename T >
struct ProcessArgType
{
	typedef typename std::conditional<
		std::is_convertible<T, RPC3*>::value
		, SetRPC3Ptr<T>
		, typename GetReadFunction<T>::type
	>::type type;
};


template<typename F>
struct RpcInvoker;

template<typename R, typename... Args>
struct RpcInvoker<R(*)(Args...)> : public RpcInvoker<R(Args...)> {
};
 
template<typename R, typename... Args>
struct RpcInvoker<R(Args...)> {
    template <typename Function>
    static inline InvokeResultCodes applyer(Function func,
                                                    InvokeArgs functionArgs) {
    	std::tuple<Args...> args;
    	InvokeResultCodes irc = IRC_SUCCESS;
    	
    	RpcInvoker<decltype(func)>::apply(func, functionArgs, args, irc);
    	
    	return irc;
    }
    
	template<std::size_t I = 0, typename Function>
	static inline typename std::enable_if<I == sizeof...(Args), void>::type
			apply(Function func, InvokeArgs &functionArgs,
							std::tuple<Args...>& args, InvokeResultCodes &irc) {
		INVOKE(func, args);
		irc = IRC_SUCCESS;
	}
	
	template<std::size_t I = 0, typename Function>
	static inline typename std::enable_if<I < sizeof...(Args), void>::type
			apply(Function func, InvokeArgs &functionArgs,
                                				std::tuple<Args...>& args,
                                                InvokeResultCodes &irc) {
        auto arg = std::get<I>(args);
        ProcessArgType<decltype(arg)>::type::apply(functionArgs, arg);
        
    	RpcInvoker<decltype(func)>::template
    	                            apply<I+1>(func, functionArgs, args, irc);
	  }
};

template<typename F>
struct RpcInvokerCpp;

template<typename R, typename... Args>
struct RpcInvokerCpp<R(*)(Args...)> : public RpcInvokerCpp<R(Args...)> {
};
 
template<typename R, typename... Args>
struct RpcInvokerCpp<R(Args...)> {
    template <typename Function>
    static inline InvokeResultCodes applyer(Function func,
                                                    InvokeArgs functionArgs) {
    	std::tuple<Args...> args;
    	InvokeResultCodes irc = IRC_SUCCESS;
    	
        auto arg = std::get<0>(args);
        arg = (decltype(arg)) *(functionArgs.thisPtr);
        
    	RpcInvokerCpp<decltype(func)>::apply(func, functionArgs, args, irc);
    	
    	return irc;
    }
    
	template<std::size_t I = 0, typename Function>
	static inline typename std::enable_if<I == sizeof...(Args), void>::type
			apply(Function func, InvokeArgs &functionArgs,
							std::tuple<Args...>& args, InvokeResultCodes &irc) {
		INVOKE(func, args);
		irc = IRC_SUCCESS;
	}
	
	template<std::size_t I = 1, typename Function>
	static inline typename std::enable_if<I < sizeof...(Args), void>::type
			apply(Function func, InvokeArgs &functionArgs,
                                				std::tuple<Args...>& args,
                                                InvokeResultCodes &irc) {
        auto arg = std::get<I>(args);
        ProcessArgType<decltype(arg)>::type::apply(functionArgs, arg);
        
    	RpcInvokerCpp<decltype(func)>::template
    	                            apply<I+1>(func, functionArgs, args, irc);
	  }
};

template <typename T>
struct DoNothing
{
	static void apply(RakNet::BitStream &bitStream, T& t)
	{
		(void) bitStream;
		(void) t;
//		printf("DoNothing\n");
	}
};

struct WriteBitstream
{
	static void applyArray(RakNet::BitStream &bitStream, RakNet::BitStream* t) {apply(bitStream,t);}
	static void apply(RakNet::BitStream &bitStream, RakNet::BitStream* t)
	{
		BitSize_t oldReadOffset = t->GetReadOffset();
		t->ResetReadPointer();
		bitStream.WriteCompressed(t->GetNumberOfBitsUsed());
		bitStream.Write(t);
		t->SetReadOffset(oldReadOffset);
	}
};

//template <typename T>
struct WritePtr
{
	template <typename T2>
	static inline void applyArray(RakNet::BitStream &bitStream, T2 *t) {bitStream << (*t);}
	template <typename T2>
	static inline void apply(RakNet::BitStream &bitStream, T2 *t) {bitStream << (*t);}
//	template <>
	static inline void apply(RakNet::BitStream &bitStream, char *t) {bitStream << t;}
//	template <>
	static inline void apply(RakNet::BitStream &bitStream, unsigned char *t) {bitStream << t;}
//	template <>
	static inline void apply(RakNet::BitStream &bitStream, const char *t) {bitStream << t;}
//	template <>
	static inline void apply(RakNet::BitStream &bitStream, const unsigned char *t) {bitStream << t;}
};

template< typename T >
struct DoWrite
{
	typedef typename std::conditional<
		std::is_convertible<T*,RakNet::BitStream*>::value,
		WriteBitstream,
		WritePtr >::type type;
};

template <typename T>
struct WriteWithNetworkIDPtr
{
	static void apply(RakNet::BitStream &bitStream, T& t)
	{
		bool isNull;
		isNull=(t==0);
		bitStream.Write(isNull);
		if (isNull)
			return;
		RPC3Tag tag;
		__RPC3ClearPtr(t, &tag);
		bool deref = (tag.flag & RPC3_TAG_FLAG_DEREF) !=0;
		bool isArray = (tag.flag & RPC3_TAG_FLAG_ARRAY) !=0;
		bitStream.Write(deref);
		bitStream.Write(isArray);
		if (isArray)
		{
			bitStream.WriteCompressed(tag.count);
		}
		for (unsigned int i=0; i < tag.count; i++)
		{
			NetworkID inNetworkID=t->GetNetworkID();
			bitStream << inNetworkID;
			if (deref)
			{
				// skip bytes, write data, go back, write number of bits written, reset cursor
				bitStream.AlignWriteToByteBoundary();
				BitSize_t writeOffset1 = bitStream.GetWriteOffset();
				BitSize_t bitsUsed1=bitStream.GetNumberOfBitsUsed();
				bitStream.Write(bitsUsed1);
				bitsUsed1=bitStream.GetNumberOfBitsUsed();
				DoWrite< typename std::remove_pointer<T>::type >::type::apply(bitStream,t);
				BitSize_t writeOffset2 = bitStream.GetWriteOffset();
				BitSize_t bitsUsed2=bitStream.GetNumberOfBitsUsed();
				bitStream.SetWriteOffset(writeOffset1);
				bitStream.Write(bitsUsed2-bitsUsed1);
				bitStream.SetWriteOffset(writeOffset2);
			}
		}
	}
};

template <typename T>
struct WriteWithoutNetworkIDNoPtr
{
	static void apply(RakNet::BitStream &bitStream, T& t)
	{
		DoWrite< typename std::remove_pointer<T>::type >::type::apply(bitStream,&t);
	}
};

template <typename T>
struct WriteWithoutNetworkIDPtr
{
	static void apply(RakNet::BitStream &bitStream, T& t)
	{
		bool isNull;
		isNull=(t==0);
		bitStream.Write(isNull);
		if (isNull)
			return;

		RPC3Tag tag;
		__RPC3ClearPtr((void*) t, &tag);
		bool isArray = (tag.flag & RPC3_TAG_FLAG_ARRAY) !=0;
		bitStream.Write(isArray);
		if (isArray)
		{
			bitStream.WriteCompressed(tag.count);
		}
		if (isArray)
		{
			for (unsigned int i=0; i < tag.count; i++)
				DoWrite< typename std::remove_pointer<T>::type >::type::applyArray(bitStream,t+i);
		}
		else
		{
			DoWrite< typename std::remove_pointer<T>::type >::type::apply(bitStream,t);
		}
		
	}
};

template <typename T>
struct SerializeCallParameterBranch
{
	typedef typename std::conditional<
		std::is_convertible<T,RPC3*>::value
		, DoNothing<T>
		, WriteWithoutNetworkIDPtr<T>
	>::type typeCheck1;

	typedef typename std::conditional<
		std::is_pointer<T>::value
		, typeCheck1
		, WriteWithoutNetworkIDNoPtr<T>
	>::type typeCheck2;

	typedef typename std::conditional<
		std::is_convertible<T,NetworkIDObject*>::value
		, WriteWithNetworkIDPtr<T>
		, typeCheck2
	>::type type;
};

/*
void Func(ARG arg1, ARG2 arg2) {
	...
}

REGISTER_FUNCTION(rpcManager, Func, ARG, ARG2)

template<typename Function, typename... ArgTypes>
void register_function(Function f, ArgTypes...) {
	GetBoundPointer<ArgTypes...>(f);
}

template<typename Function>
struct BoostRPCInvoker {
	template<typename... ArgTypes>
	void apply(Function func, InvokeArgs &functionArgs, ArgTypes...) {}
}


using placeholders_list = std::tuple<decltype(std::placeholders::_1)
								   , decltype(std::placeholders::_2)
                                   , decltype(std::placeholders::_3)
                                   , decltype(std::placeholders::_4)
                                   , decltype(std::placeholders::_5)
                                   , decltype(std::placeholders::_6)
                                   , decltype(std::placeholders::_7)
                                   , decltype(std::placeholders::_8)
                                   , decltype(std::placeholders::_9)
                                   , decltype(std::placeholders::_10)
                                   , decltype(std::placeholders::_11)
                                   >;

template<typename... ArgTypes>
FunctionPointer func(bool isCppPointer, ArgTypes... args) {
	return std::make_tuple(isCppPointer, std::bind( & BoostRPCInvoker<Function, ArgTypes...>::apply, f, std::placeholders::_1));
}

template <std::size_t... Ints>
FunctionPointer prefunc(bool isCppPointer, std::index_sequence<Ints...>) {
    return func(isCppPointer, typename std::tuple_element<Ints, placeholders_list>::type{}...);
}*/
							   
template<typename Function>
struct GetBoundPointer_C
{
//	typedef typename GetBoundPointer_C type;
	static FunctionPointer GetBoundPointer(Function f)
	{
        //InvokeResultCodes(RpcInvoker<decltype(f)>::*invoke_func)(InvokeArgs) = &RpcInvoker<decltype(f)>::applyer;
		return std::make_tuple(false, std::bind( static_cast<InvokeResultCodes(*)(Function, InvokeArgs)>(&RpcInvoker<decltype(f)>::applyer), f, std::placeholders::_1));
		//return std::make_tuple(false, std::bind( & BoostRPCInvoker_First<Function>::apply, f, std::placeholders::_1));
		//const int n = sizeof...(ArgTypes);
		//return prefunc(false, std::make_index_sequence<n>{});
		//return std::make_tuple(false, std::bind( & BoostRPCInvoker<Function>::apply<ArgTypes...>, f, std::placeholders::_1, typename std::tuple_element<n, placeholders_list>::type{}std::make_index_sequence<n>{}));
	}
};

template<typename Function>
struct GetBoundPointer_CPP
{
//	typedef typename GetBoundPointer_CPP type;
	static FunctionPointer GetBoundPointer(Function f)
	{
        //= &RpcInvoker<decltype(f)>::applyer;
		return std::make_tuple(true, std::bind( static_cast<InvokeResultCodes(*)(Function, InvokeArgs)>(&RpcInvokerCpp<decltype(f)>::applyer), f, std::placeholders::_1));
        //return std::bind( static_cast<int(*)(int, float, std::string, Function)>(&dd::ff), 1, 2.0, std::placeholders::_1, f);
        
		//const int n = sizeof...(ArgTypes) + 1;
		//return prefunc(true, std::make_index_sequence<n>{});
		//  apply<EMPTY_SEQUENCE? OR char*>  --   std::iterator_traits
		//typedef std::function<Function> func;
		//return std::make_tuple(true, std::bind( & BoostRPCInvoker_ThisPtr<Function>::apply<std::placeholders::_2>, f, std::placeholders::_1, std::placeholders::_2 ));
	}
};


template<typename Function>
FunctionPointer GetBoundPointer(Function f)
{
	return std::conditional<
	std::is_member_function_pointer<Function>::value
	, GetBoundPointer_CPP<Function>
	, GetBoundPointer_C<Function>
	>::type::GetBoundPointer(f);
	
//	return FunctionPointer(true, boost::bind( & BoostRPCInvoker<Function>::apply<boost::fusion::nil>, f, std::placeholders::_1, boost::fusion::nil() ) );
}


}
}

#endif
