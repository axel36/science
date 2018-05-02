#pragma once
#include <iostream>

template<class T>
class Mvector
{
public:
	// Вспомогательные части для вектора
	Mvector();
	Mvector(int l);
	Mvector(const Mvector<T>& other);
	Mvector(Mvector<T>&& moved);
	Mvector<T>& operator=(const Mvector<T>& other);
	Mvector<T>& operator=(Mvector<T>&& moved);
	~Mvector();

	T& operator[](size_t i);
	const T& operator[](size_t i) const;
	void push_front(const T& x);
	size_t size() const;
	size_t capacity();

	// Требуемые функции 
	void push_back(const T& x);
	void insert(size_t index, const T&);
	void remove(int index);
	T set(size_t index, const T&);
	T& at(int index);


private:
	T * _array = nullptr;
	size_t _size;
	size_t _capacity;
	void resize();
};

template<class T>
Mvector<T>::Mvector()
{
	_capacity = 4;
	_array = new T[_capacity];
	_size = 0;
}

template<class T>
Mvector<T>::Mvector(int l)
{
	_capacity = l;
	_array = new T[_capacity];
	for (int i = 0; i < l; i++) {
		_array[i] = T{};
	}
	_size = l;
}

template<class T>
Mvector<T>::Mvector(const Mvector<T> & other) :
	_capacity(other._capacity), _size(other._size)
{
	_array = new T[_capacity];
	std::copy(other._array, other._array + _size, _array);
}

template<class T>
Mvector<T> & Mvector<T>::operator=(const Mvector<T> & other)
{
	if (this != &other) {
		T* tmp = new T[other._capacity];
		std::copy(other._array, other._array + other._size, tmp);
		delete[] _array;
		_array = tmp;
		_capacity = other._capacity;
		_size = other._size;
	}
	return *this;
}

template<class T>
Mvector<T>::Mvector(Mvector && moved) :
	_capacity(moved._capacity), _size(moved._size)
{
	_array = moved._array;
	moved._array = nullptr;
}

template<class T>
Mvector<T>& Mvector<T>::operator=(Mvector<T>&& moved)
{
	if (this != &moved) {
		delete[] _array;
		_array = moved._array;
		_capacity = moved._capacity;
		_size = moved._size;
		moved._array = nullptr;
	}
	return *this;
}

template<class T>
Mvector<T>::~Mvector()
{

	delete[] _array;
}

template<class T>
T& Mvector<T>::operator[](size_t i)
{
	return _array[i];
}

template<class T>
const T& Mvector<T>::operator[](size_t i) const
{
	return _array[i];
}

template<class T>
void Mvector<T>::push_back(const T& x)
{
	if (_size == _capacity) {
		resize();
	}
	_array[_size++] = x;
}


template<class T>
inline void Mvector<T>::push_front(const T& x)
{
	if (_size == _capacity) {
		resize();
	}
	if (_size > 0) {
		for (size_t i = _size; i > 0; i--) {
			_array[i] = _array[i - 1];
		}
	}
	_size++;
	_array[0] = x;
}

template<class T>
size_t Mvector<T>::size() const
{
	return _size;
}

template<class T>
size_t Mvector<T>::capacity()
{
	return _capacity;
}

template<class T>
void Mvector<T>::insert(size_t index, const T & obj)
{
	_array[index] = obj;
}

template<class T>
void Mvector<T>::remove(int index)
{
	(_array + index)->~T();
	_array[index] = T{};
}

template<class T>
T  Mvector<T>::set(size_t index, const T & obj)
{
	T tmp = _array[index];
	_array[index] = obj;
	return tmp;
}

template<class T>
T& Mvector<T>::at(int index)
{
	if (index > _size) {
		throw std::out_of_range("wrong index");
	}
	return _array[index];
}

template<class T>
void Mvector<T>::resize()
{
	_capacity *= 2;
	T* newMem = new T[_capacity];
	std::copy(_array, _array + _size, newMem);
	delete[] _array;
	_array = newMem;
}
